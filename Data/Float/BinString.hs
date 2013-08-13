-- |
--  Module:     Data.Float.BinString
--  License:    BSD-style
--  Maintainer: me@lelf.lu
--
--
-- This module contains functions for formatting and parsing floating point
-- values as C99 printf/scanf functions with format string @%a@ do.
--
-- Format is [-]0x/h.hhhhh/p±/ddd/, where /h.hhhhh/ is significand as
-- a hexadecimal floating-point number and /±ddd/ is exponent as a
-- decimal number. Significand has as many digits as needed to exactly
-- represent the value, fractional part may be ommitted.
-- 
-- Infinity and NaN are represented as @±inf@ and @nan@ accordingly.
-- 
-- For example, @(π ∷ Double) = 0x1.921fb54442d18p+1@.
--
-- Assertion
-- 
--     @Just x ≡ readFloatStr (showFloatStr x)@
-- 
-- holds (modulo bugs and cosmic rays).
-- 
-- Floating point radix is assumed to be 2.

{-# LANGUAGE Safe #-}
{-# LANGUAGE TupleSections #-}
module Data.Float.BinString (showFloatStr,readFloatStr) where

import Numeric
import Data.List.Split
import Data.Char
import Text.Parsec
import Control.Applicative ((<**>))


floatToHexDigits x = (,ep') $ d0 : map to16 chunked
    where ((d0:ds),ep) = floatToDigits 2 x
          ep' | x == 0    = ep
              | otherwise = ep-1
          chunked = map (take 4 . (++ repeat 0)) . chunksOf 4 $ ds
          to16 = foldl1 (\a b -> 2*a+b)


-- | Format a value. Will provide enough digits to reconstruct the value exactly.
showFloatStr :: RealFloat a => a -> String
showFloatStr x | isNaN x      = "nan"
               | isInfinite x = sign ++ "inf"
               | otherwise    = sign ++ "0x"
                                ++ [ intToDigit $ head digs ]
                                ++ [ '.' | length digs > 1 ]
                                ++ (map intToDigit $ tail digs)
                                ++ "p" ++ (if ep>=0 then "+" else "-") ++ show (abs ep)
    where (digs,ep) = floatToHexDigits $ abs x
          sign      = [ '-' | x < 0 ]


data Sign = Pos | Neg deriving Show
data ParsedFloat = Inf Sign | NaN | Float Sign String Sign String
                   deriving Show

signed Pos x = x
signed Neg x = -x


-- | Parse a value from 'String'.
readFloatStr :: RealFloat a => String -> Maybe a
readFloatStr s = either (const Nothing) (Just . decode) pd
    where pd = parse parser "" s

decode (Float sgn digs exp_sgn exp_digs) = signif * 2^^expon
    where signif = signed sgn $ (fst $ head $ readHex digs) / 16^^(length digs - 1)
          expon  = signed exp_sgn $ read exp_digs

decode NaN = 0/0
decode (Inf sgn) = signed sgn $ 1/0

parserPeculiar = do sgn <- optSign
                    (string "nan" >> return NaN) <|> (string "inf" >> return (Inf sgn))

parserPeculiar' = optSign <**> ((string "nan" >> return (const NaN))
                                <|> (string "inf" >> return Inf))



parserNormal = do positive <- optSign
                  string "0x"
                  digit0 <- hexDigit
                  restDigits <- option [] $ char '.' >> many hexDigit
                  char 'p'
                  positiveExp <- optSign
                  expDigits <- many digit
                  return $ Float positive (digit0:restDigits) positiveExp expDigits

optSign = option Pos $ (char '+' >> return Pos) <|> (char '-' >> return Neg)
-- hexDigitD = hexDigit >>= \c -> return $ digitToInt c
-- digitD = do { d <- digit; return $ digitToInt d }

parser = do r <- try parserPeculiar <|> parserNormal
            eof
            return r

main = putStrLn "hi"


-- parse (d0:ds) exp = m * 2**exp
--     where m = d0 + foldr (\r x -> (r+x)/16) 0 ds


