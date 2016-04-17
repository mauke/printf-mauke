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

{-# LANGUAGE TemplateHaskell #-}

module Text.Printf.Mauke.TH (printf, sprintf) where

import Text.Printf.Mauke.Internal

import Data.Char

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL

import Language.Haskell.TH

xprintf :: ExpQ -> String -> ExpQ
xprintf fini fmt = do
    ps <- params fmt
    gen fini [| vprintf fmt |] ps id

gen :: ExpQ -> ExpQ -> [PType] -> (ExpQ -> ExpQ) -> ExpQ
gen fini z = go
    where
    go [] = \f -> [| $fini ($z $(f [| [] |])) |]
    go (x : xs) =
        let g = go xs in
        \f -> [| \a -> $(g (\as -> f [| $(tembed x) a : $as |])) |]

-- | A static checking layer on top of 'Text.Printf.Mauke.printf'. It hasn't
-- been tested much, but static argument checking is always a good idea. To use
-- it, add
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > import Text.Printf.Mauke.TH
--
-- at the top of your code and call @$('printf' \"%d %d\") x y@ instead of
-- @'Text.Printf.Mauke.printf' \"%d %d\" x y@.
--
-- Starting with version 0.6.0 of this library, 'printf' \'s return type is
-- overloaded: Like the non-TH 'Text.Printf.Mauke.printf' it can return either
-- a 'String' or @'IO' a@. (It used to return 'String' unconditionally.)

printf :: String -> ExpQ
printf = xprintf [| output |]

-- | Works like 'printf' but always returns a 'String'. In versions before
-- 0.6.0 of this library, this function was called @printf@.

sprintf :: String -> ExpQ
sprintf = xprintf [| id |]

data PType = I | C | S | F | X
    deriving (Eq, Ord, Show, Read)

params :: String -> Q [PType]
params "" = return []
params ('%' : cs) = case dropWhile (`elem` " +-0#") cs of
    '*' : 'v' : cs' -> fmap (S :) $ step1 True cs'
    'v' : cs' -> step1 True cs'
    cs' -> step1 False cs'
    where
    step1 mt xs = case xs of
        '*' : xs' -> fmap (I :) $ step2 mt xs'
        xs' -> step2 mt (dropWhile isDigit xs')
    step2 mt xs = case xs of
        '.' : '*' : xs' -> fmap (I :) $ step3 mt xs'
        '.' : xs' -> step3 mt (dropWhile isDigit xs')
        _ -> step3 mt xs
    step3 mt xs = case xs of
        "" -> fail "unterminated formatting directive"
        '%' : xs' -> params xs'
        x : xs'
            | x == 'c' -> fmap ((if mt then S else C) :) $ params xs'
            | x `elem` "duoOxXbB" -> fmap ((if mt then S else I) :) $ params xs'
            | x == '_' -> fmap ((if mt then S else X) :) $ params xs'
            | x == 's' ->
                if mt
                then fail "v flag invalid for %s"
                else fmap (S :) $ params xs'
            | x `elem` "eEfFgG" ->
                if mt
                then fail $ "v flag invalid for %" ++ [x]
                else fmap (F :) $ params xs'
            | otherwise -> fail $ "invalid format specifier: " ++ show x
params (_ : xs) = params xs

tembed :: PType -> ExpQ
tembed t = case t of
    I -> [| AInt . toInteger |]
    C -> [| AChar |]
    S -> [| AStr . toString |]
    F -> [| AFloat . realToFrac |]
    X -> [| embed |]

class ToString a where
    toString :: a -> String

instance (ToChar a) => ToString [a] where
    toString = map toChar

instance ToString BS.ByteString where
    toString = BS.unpack

instance ToString BL.ByteString where
    toString = BL.unpack

class PrintfResult a where
    output :: String -> a

instance (FromChar a) => PrintfResult [a] where
    output = map fromChar

instance PrintfResult (IO a) where
    output s = putStr s >> return (die "TH.printf" "undefined")
