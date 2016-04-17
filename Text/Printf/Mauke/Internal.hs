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

{-# LANGUAGE CPP #-}

-- | Implementation details. You don't see this.
module Text.Printf.Mauke.Internal (
    vprintf,
    Arg(..),
    PrintfArg(..),
    FromChar(..),
    ToChar(..),
    die,
    pdie
) where

import Control.Monad

import Data.Char
import Data.Default
import Data.List
import Data.Maybe
import Data.Ratio

import Numeric

import qualified Data.Set as Set
import Data.Set (Set)

import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.C.Types
import System.Posix.Types

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL

thisModule :: String
thisModule = "Text.Printf.Mauke"

die :: String -> String -> a
die f s = error $ concat [thisModule, ".", f, ": ", s]

pdie :: String -> a
pdie = die "printf"

class FromChar a where
    fromChar :: Char -> a

instance FromChar Char where
    fromChar = id

class ToChar a where
    toChar :: a -> Char

instance ToChar Char where
    toChar = id

instance ToChar Word8 where
    toChar = chr . fromIntegral

-- | The internal type used to wrap and store all arguments.
data Arg
    = AInt Integer
    | AChar Char
    | AStr String
    | AFloat Double
    deriving (Eq, Ord, Show, Read)

ashow :: Arg -> String
ashow (AInt i) = show i
ashow (AChar c) = show c
ashow (AStr s) = show s
ashow (AFloat d) = show d

-- | Class for valid printf arguments.
class PrintfArg a where
    embed :: a -> Arg

instance PrintfArg Char where
    embed = AChar

instance (ToChar a) => PrintfArg [a] where
    embed = AStr . map toChar

instance PrintfArg BS.ByteString where
    embed = AStr . BS.unpack

instance PrintfArg BL.ByteString where
    embed = AStr . BL.unpack

instance PrintfArg Double where
    embed = AFloat

instance PrintfArg Float where
    embed = AFloat . realToFrac

instance PrintfArg CFloat where
    embed = AFloat . realToFrac

instance PrintfArg CDouble where
    embed = AFloat . realToFrac

#if 0
instance PrintfArg CLDouble where
    embed = AFloat . realToFrac
#endif

instance (Integral a) => PrintfArg (Ratio a) where
    embed = AFloat . realToFrac

instance PrintfArg Integer where
    embed = AInt

instance PrintfArg Int where
    embed = AInt . fromIntegral

instance PrintfArg Int8 where
    embed = AInt . fromIntegral

instance PrintfArg Int16 where
    embed = AInt . fromIntegral

instance PrintfArg Int32 where
    embed = AInt . fromIntegral

instance PrintfArg Int64 where
    embed = AInt . fromIntegral

instance PrintfArg Word where
    embed = AInt . fromIntegral

instance PrintfArg Word8 where
    embed = AInt . fromIntegral

instance PrintfArg Word16 where
    embed = AInt . fromIntegral

instance PrintfArg Word32 where
    embed = AInt . fromIntegral

instance PrintfArg Word64 where
    embed = AInt . fromIntegral

instance PrintfArg IntPtr where
    embed = AInt . fromIntegral

instance PrintfArg WordPtr where
    embed = AInt . fromIntegral

instance PrintfArg CChar where
    embed = AInt . fromIntegral

instance PrintfArg CSChar where
    embed = AInt . fromIntegral

instance PrintfArg CUChar where
    embed = AInt . fromIntegral

instance PrintfArg CShort where
    embed = AInt . fromIntegral

instance PrintfArg CUShort where
    embed = AInt . fromIntegral

instance PrintfArg CInt where
    embed = AInt . fromIntegral

instance PrintfArg CUInt where
    embed = AInt . fromIntegral

instance PrintfArg CLong where
    embed = AInt . fromIntegral

instance PrintfArg CULong where
    embed = AInt . fromIntegral

instance PrintfArg CPtrdiff where
    embed = AInt . fromIntegral

instance PrintfArg CSize where
    embed = AInt . fromIntegral

instance PrintfArg CWchar where
    embed = AInt . fromIntegral

instance PrintfArg CSigAtomic where
    embed = AInt . fromIntegral

instance PrintfArg CLLong where
    embed = AInt . fromIntegral

instance PrintfArg CULLong where
    embed = AInt . fromIntegral

instance PrintfArg CIntPtr where
    embed = AInt . fromIntegral

instance PrintfArg CUIntPtr where
    embed = AInt . fromIntegral

instance PrintfArg CIntMax where
    embed = AInt . fromIntegral

instance PrintfArg CUIntMax where
    embed = AInt . fromIntegral

instance PrintfArg CIno where
    embed = AInt . fromIntegral

instance PrintfArg CMode where
    embed = AInt . fromIntegral

instance PrintfArg COff where
    embed = AInt . fromIntegral

instance PrintfArg CPid where
    embed = AInt . fromIntegral

instance PrintfArg CSsize where
    embed = AInt . fromIntegral

instance PrintfArg Fd where
    embed = AInt . fromIntegral

#ifdef __unix__
instance PrintfArg CGid where
    embed = AInt . fromIntegral

instance PrintfArg CNlink where
    embed = AInt . fromIntegral

instance PrintfArg CUid where
    embed = AInt . fromIntegral

instance PrintfArg CTcflag where
    embed = AInt . fromIntegral

instance PrintfArg CRLim where
    embed = AInt . fromIntegral
#endif

vprintf :: String -> [Arg] -> String
vprintf "" [] = ""
vprintf "" (x : _) = die "printf" $ "excess argument: " ++ ashow x
vprintf ('%' : fmt) args =
    let
        (spec, fmt', args') = parse fmt args
        (args'', ss) = apply spec args'
    in
    ss $ vprintf fmt' args''
vprintf (c : fmt) args = c : vprintf fmt args

data Spec = Spec{
    flags :: !(Set Flag),
    vector :: !(Maybe String),
    width :: !Integer,
    precision :: !(Maybe Integer),
    ftype :: !Type
} deriving (Eq, Ord, Show, Read)

instance Default Spec where
    def = Spec def def def def def

data Flag = FSpace | FPlus | FZero | FAlt
    deriving (Eq, Ord, Show, Read)

ch2flag :: Char -> Flag
ch2flag c = case c of
    ' ' -> FSpace
    '+' -> FPlus
    '0' -> FZero
    '#' -> FAlt
    _ -> die "ch2flag" $ "internal error: " ++ show c

data Type
    = Tpercent
    | Tc | Ts | Td | Tu | To | Tx | Te | Tf | Tg
    | TO | TX | TE | TG | Tb | TB
    | Tany
    deriving (Eq, Ord, Show, Read)

instance Default Type where
    def = Tany

ch2type :: Char -> Type
ch2type c = case c of
    '%' -> Tpercent
    'c' -> Tc
    's' -> Ts
    'd' -> Td
    'u' -> Tu
    'o' -> To
    'O' -> TO
    'x' -> Tx
    'X' -> TX
    'e' -> Te
    'f' -> Tf
    'g' -> Tg
    'E' -> TE
    'G' -> TG
    'b' -> Tb
    'B' -> TB
    '_' -> Tany
    _ -> pdie $ "invalid format specifier: " ++ show c

enoarg :: a
enoarg = pdie "missing argument"

auncons :: [Arg] -> (Arg, [Arg])
auncons [] = enoarg
auncons (x : xs) = (x, xs)

arg2int :: Arg -> Integer
arg2int (AInt i) = i
arg2int x = pdie $ "invalid argument: expected int, got " ++ ashow x

arg2int' :: Arg -> Integer
arg2int' (AInt i) = i
arg2int' (AChar c) = fromIntegral $ ord c
arg2int' x = pdie $ "invalid argument: expected int, got " ++ ashow x

arg2str :: Arg -> String
arg2str (AStr s) = s
arg2str x = pdie $ "invalid argument: expected string, got " ++ ashow x

arg2float :: Arg -> Double
arg2float (AFloat f) = f
arg2float x = pdie $ "invalid argument: expected float, got " ++ ashow x

parseInt :: String -> [Arg] -> (Maybe Integer, String, [Arg])
parseInt str args = case str of
    '*' : str' ->
        let (arg, args') = auncons args in
        (Just $ arg2int arg, str', args')
    _ ->
        let (d, str') = span (\c -> c >= '0' && c <= '9') str in
        (if null d then Nothing else Just $ read d, str', args)

parseVec :: String -> [Arg] -> (Maybe String, String, [Arg])
parseVec str args = case str of
    'v' : str' -> (Just ".", str', args)
    '*' : 'v' : str' -> (Just sa, str', args')
    _ -> (Nothing, str, args)
    where
    (arg, args') = auncons args
    sa = arg2str arg

parse :: String -> [Arg] -> (Spec, String, [Arg])
parse s args =
    let
        (fch, s1) = span (`elem` " +-0#") s
        fl = Set.fromList . map ch2flag . filter ('-' /=) $ fch
        (vc, s2, args1) = parseVec s1 args
        (wd, s3, args2) = parseInt s2 args1
        (pr, s4, args3) = case s3 of
            '.' : t ->
                let (mi, str, ar) = parseInt t args2 in
                (mi `mplus` Just 0, str, ar)
            _ -> (Nothing, s3, args2)
        (tp, s5) = case s4 of
            "" -> pdie $ "unterminated formatting directive"
            c : cs -> (ch2type c, cs)
    in (
        def{
            flags = fl,
            vector = vc,
            width = (if '-' `elem` fch then negate else id) . fromMaybe 0 $ wd,
            precision = pr,
            ftype = tp
        },
        s5,
        args3
    )

padWith :: a -> Integer -> [a] -> [a]
padWith c w s
    | w <= 0 = lgo (negate w) s
    | otherwise = genericReplicate (missingFrom w s) c ++ s
    where
    lgo n xs | n <= 0 = xs
    lgo n [] = genericReplicate n c
    lgo n (x : xs) = x : lgo (pred n) xs
    missingFrom n _ | n <= 0 = 0
    missingFrom n [] = n
    missingFrom n (_ : xs) = missingFrom (pred n) xs

padChar :: Spec -> Char
padChar spc
    | FZero `Set.member` flags spc
    && width spc > 0
    && (
        isNothing (precision spc) ||
        ftype spc `notElem` [Td, Tu, To, Tx, TX, Tb, TB]
    ) = '0'
    | otherwise = ' '

int2char :: Integer -> Char
int2char i
    | i < lo || i > hi = '\xfffd'
    | otherwise = chr (fromInteger i)
    where
    lo = fromIntegral $ ord minBound
    hi = fromIntegral $ ord maxBound

dropSuffix :: (Eq a) => [a] -> [a] -> [a]
dropSuffix t xs | t == xs = []
dropSuffix t (x : xs) = x : dropSuffix t xs
dropSuffix _ [] = []

apply :: Spec -> [Arg] -> ([Arg], String -> String)
apply spc args
    | isJust (vector spc) =
        let Just d = vector spc in
        args' <&>
            ($ "") . foldr (.) id . intersperse (d ++) . map (snd . apply spc{ vector = Nothing } . return . embed) $ arg2str arg
    | otherwise = case ftype spc of
        Tpercent -> args <&> "%"
        Tc -> args' <&> [int2char argi]
        Ts -> args' <&> maybe id genericTake (precision spc) . arg2str $ arg
        Tu -> args' <&>
            maybe id (padWith '0' . max 0) (precision spc) $ show argu
        Td -> ifmt show
        To -> ifmt $ showBase 8
        TO -> ifmt $ showBase 8
        Tx -> ifmt $ showBase 16
        TX -> ifmt $ uc . showBase 16
        Tb -> ifmt $ showBase 2
        TB -> ifmt $ showBase 2
        Tf -> ffmt . dF $ showFFloat fprec
        Te -> ffmt . dF $ showEFloat fprec
        TE -> ffmt . (uc .) . dF $ showEFloat fprec
        Tg -> ffmt . (dropSuffix ".0" .) . dF $ showGFloat (fmap fromIntegral $ precision spc)
        TG -> ffmt . ((uc . dropSuffix ".0") .) . dF $ showGFloat (fmap fromIntegral $ precision spc)
        Tany ->
            spc{
                ftype = case arg of
                    AInt{} -> Td
                    AChar{} -> Tc
                    AStr{} -> Ts
                    AFloat{} -> Tg
            } `apply` args
    where
    uc = map toUpper
    showBase b n = showIntAtBase b intToDigit n ""
    dF f = flip f ""
    infixr 0 <&>
    x <&> y = (x, (pad y ++))
    pC = padChar spc
    pad = padWith pC (width spc)
    (arg, args') = auncons args
    argf = arg2float arg
    fprec = Just $ maybe 6 fromIntegral (precision spc)
    fprefix
        | argf < 0 = "-"
        | FPlus `Set.member` flags spc = "+"
        | FSpace `Set.member` flags spc = " "
        | otherwise = ""
    argi = arg2int' arg
    argu
        | argi < 0 = pdie $ "invalid argument: expected unsigned int, got " ++ show argi
        | otherwise = argi
    arga = abs argi
    iprefix =
        case () of
        _
            | argi < 0 -> "-"
            | FPlus `Set.member` flags spc -> "+"
            | FSpace `Set.member` flags spc -> " "
            | otherwise -> ""
        ++
        if FAlt `Set.notMember` flags spc then ""
        else case ftype spc of
            To -> "0o"
            TO -> "0O"
            Tx -> "0x"
            TX -> "0X"
            Tb -> "0b"
            TB -> "0B"
            _ -> ""
    ifmt pp = (,) args' . (++) $
        (if pC /= '0' then pad else id) $
        iprefix ++
        maybe
            (
                if pC == '0'
                then padWith '0' (max 0 $ width spc - fromIntegral (length iprefix))
                else id
            )
            (padWith '0' . max 0)
            (precision spc)
            (pp arga)
    ffmt pp = (,) args' . (++) $
        case () of
        _
            | isNaN argf -> padWith ' ' (width spc) $ fprefix ++ "nan"
            | isInfinite argf -> padWith ' ' (width spc) $ fprefix ++ "inf"
            | otherwise ->
                (if pC /= '0' then pad else id) $
                fprefix ++
                (
                    if pC == '0'
                    then padWith '0' (max 0 $ width spc - fromIntegral (length fprefix))
                    else id
                ) (pp $ abs argf)
