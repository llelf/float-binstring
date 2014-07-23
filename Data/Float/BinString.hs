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

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Data.Float.BinString (readFloat,showFloat,floatBuilder,
                             readFloatStr,showFloatStr) where

import qualified Numeric as Numeric
import Data.List.Split
import Data.Char

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int (decimal)

import Data.Attoparsec.Text hiding (take,signed,decimal)
import Control.Applicative ((<**>),(<|>),many)
import Data.Monoid


floatToHexDigits x = (,ep') $ d0 : map to16 chunked
    where ((d0:ds),ep) = Numeric.floatToDigits 2 x
          ep' | x == 0    = ep
              | otherwise = ep-1
          chunked = map (take 4 . (++ repeat 0)) . chunksOf 4 $ ds
          to16 = foldl1 (\a b -> 2*a+b)


-- | Format a value. Will provide enough digits to reconstruct the value exactly.
showFloat :: RealFloat a => a -> Text
showFloat = toStrict . toLazyText . floatBuilder


floatBuilder :: RealFloat a => a -> Builder
floatBuilder x | isNaN x      = fromText "nan"
               | isInfinite x = sign <> fromText "inf"
               | otherwise    = sign <> fromText "0x"
                                <> singleton (intToDigit d0)
                                <> fromString [ '.' | length digs > 1 ]
                                <> fromString [ intToDigit x | x <- dtail]
                                <> singleton 'p'
                                <> singleton (if ep>=0 then '+' else '-')
                                <> decimal (abs ep)
    where (digs,ep)  = floatToHexDigits $ abs x
          (d0:dtail) = digs
          sign       = fromString [ '-' | x < 0 ]


{-# DEPRECATED readFloatStr "use readFloat" #-}
readFloatStr :: RealFloat a => String -> Maybe a
readFloatStr = readFloat . T.pack

{-# DEPRECATED showFloatStr "use showFloat" #-}
showFloatStr :: RealFloat a => a -> String
showFloatStr = T.unpack . showFloat


data Sign = Pos | Neg deriving Show
data ParsedFloat = Inf Sign | NaN | Float Sign String Sign String
                   deriving Show

signed Pos x = x
signed Neg x = -x


-- | Parse a value from 'Text'.
readFloat :: RealFloat a => Text -> Maybe a
readFloat s = either (const Nothing) (Just . decode) pd
    where pd = parseOnly parser s

decode :: (Eq a, Fractional a) => ParsedFloat -> a
decode (Float sgn digs exp_sgn exp_digs) = signif * 2^^expon
    where signif = signed sgn v / 16^^(length digs - 1)
          [(v,_)] = Numeric.readHex digs
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

hexDigit = satisfy isHexDigit


optSign = option Pos $ (char '+' >> return Pos) <|> (char '-' >> return Neg)


parser = do r <- try parserPeculiar <|> parserNormal
            endOfInput
            return r


