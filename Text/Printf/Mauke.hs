{-

Copyright (c) 2013 Lukas Mai

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.
* Neither the name of the author nor the names of his contributors
  may be used to endorse or promote products derived from this software
  without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY LUKAS MAI AND CONTRIBUTORS "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

module Text.Printf.Mauke (
    printf,
    hPrintf,
    PrintfArg(..),
    Arg(..),
    FromChar(..),
    ToChar(..),
    PrintfType,
    HPrintfType
) where

import Text.Printf.Mauke.Internal

import System.IO

-- | Format a variable number of arguments according to a format string,
-- similar to (s)printf in Perl. The return value is either a 'String' or
-- @'IO' a@, in which case the result is printed to 'stdout'. If you use the
-- 'IO' variant, don't use the result: Because it has to be of any type @a@,
-- this library makes it 'undefined'.
--
-- The format string consists of ordinary characters (everything except
-- @\'%\'@), which are passed through unchanged, and formatting directives,
-- which have the following form:
--
-- @
--  % /flag/* /vector/? /width/? /precision/? /type/
-- @
--
-- (@*@ and @?@ mean 0 or more and 0 or 1 of the preceding item, respectively.)
--
-- Flags:
--
-- [@space@] prefix positive numbers with a space
--
-- [@+@] prefix positive numbers with a plus sign (overrides space if both are
-- present)
--
-- [@-@] left-justify within the field
--
-- [@0@] pad with zeroes on the left, not spaces
--
-- [@#@] prefix binary numbers with @0b@\/@0B@, octal numbers with @0o@\/@0O@
-- and hexadecimal numbers with @0x@\/@0X@
--
-- The /vector/ flag @v@ tells 'printf' to format each character in the string
-- argument according to the current directive, then joins the results with a
-- separator that defaults to @\".\"@. When @*v@ is used, the separator is
-- taken from the argument list (use e.g. @'printf' \"%*v.2x\" \"\" str@ if you
-- want no separator).
--
-- The /width/ is either a decimal integer or @*@, in which case the width is
-- taken from the argument list (this argument must be an integer). It
-- specifies the minimum width for this field. Shorter values are padded on
-- the left with spaces (but this can be changed by the @0@ and @-@ flags). If
-- the /width/ taken from the argument list is negative, it behaves as if the
-- @-@ flag was specified.
--
-- The /precision/ consists of a @.@ followed by digits or a @*@ (see the
-- description of /width/ above). The effects depend on the format /type/:
--
-- * for floating point formats, this specifies the number of digits after the
-- decimal point
--
-- * for string formats, this is the maximum number of characters to appear
-- (longer strings are truncated)
--
-- * for integer formats, this is the minimum number of digits to appear in
-- the output; shorter values are zero-padded
--
-- Types:
--
-- [@%@] A percent sign. No argument is consumed.
--
-- [@c@] A character. If the argument is an integer, it is converted with
-- 'chr'.
--
-- [@s@] A string.
--
-- [@d@] A decimal integer.
--
-- [@u@] An unsigned decimal integer.
--
-- [@b@] A binary integer.
--
-- [@B@] Like @b@, but using a @0B@ prefix with @#@.
--
-- [@o@] An octal integer.
--
-- [@O@] Like @o@, but using a @0O@ prefix with @#@.
--
-- [@x@] A hexadecimal integer.
--
-- [@X@] Like @x@, but using uppercase letters.
--
-- [@e@] A floating point number in scientific notation.
--
-- [@E@] Like @e@, but using an uppercase @E@.
--
-- [@f@] A floating point number in fixed decimal notation.
--
-- [@g@] A floating point number in @%e@ or @%f@ notation.
--
-- [@G@] Like @g@, but using an uppercase @E@.
--
-- [@_@] A generic format; it behaves like @%c@, @%s@, @%d@ or @%g@, depending
-- on the argument type.
printf :: (PrintfType r) => String -> r
printf = collect id

class PrintfType a where
    collect :: ([Arg] -> [Arg]) -> String -> a

instance (FromChar a) => PrintfType [a] where
    collect arg fmt = map fromChar $ vprintf fmt (arg [])

instance (PrintfArg a, PrintfType r) => PrintfType (a -> r) where
    collect arg fmt x = collect (arg . (embed x :)) fmt

instance PrintfType (IO a) where
    collect arg fmt = putStr (collect arg fmt) >> return (pdie undefinedResult)

undefinedResult :: String
undefinedResult = "the return value is a lie"

-- | Like 'printf', except that the result is printed to the specified
-- 'Handle'.
hPrintf :: (HPrintfType r) => Handle -> String -> r
hPrintf h = hcollect h id

class HPrintfType a where
    hcollect :: Handle -> ([Arg] -> [Arg]) -> String -> a

instance (PrintfArg a, HPrintfType r) => HPrintfType (a -> r) where
    hcollect h arg fmt x = hcollect h (arg . (embed x :)) fmt

instance HPrintfType (IO a) where
    hcollect h arg fmt = hPutStr h (vprintf fmt (arg [])) >> return (die "hPrintf" undefinedResult)
