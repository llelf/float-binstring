--module Data.Float.BinString.Test where
{-# LANGUAGE OverloadedStrings #-}

import Data.Float.BinString

import Control.Applicative
import Data.Text (pack,unpack,Text)
import Control.Monad
import Data.Monoid

import Test.Hspec
import Test.QuickCheck
import Test.HUnit
import Test.Hspec.HUnit

import Debug.Trace


rev_prop x = Just True == liftA2 eq (Just x) (readFloat (showFloat x))


eq :: Double -> Double -> Bool
eq a b | isNaN a, isNaN b = True
       | otherwise        = a == b

nan = 0/0



main = hspec $ do
         describe "triv read" $ do
           it "0" $ readFloat "0x0p0" @?= Just 0.0
           it "1" $ readFloat "0x1p0" @?= Just 1.0
           it "-0" $ readFloat "-0x0p0" @?= Just (-0.0)
           it "inf" $ readFloat "inf" @?= Just (1/0)

         describe "triv show" $ do
           it "0" $ showFloat 0 @?= "0x0p+0"
           it "pi" $ showFloat pi @?= "0x1.921fb54442d18p+1"

         describe "quickcheck" $ do
           it "rev" $ property rev_prop

         describe "predef" $ do
           it "..." $ predefined



predefined = do c <- readFile "tests.pairs.10k"
                let vs = map parse $ lines c
                forM_ vs $ \(x,s) ->
                    do liftA2 eq (readFloat s) (Just x) `shouldBe` Just True
                       showFloat x `shouldBe` s

  where parse s = (readF a, pack b) :: (Double,Text) where [a,b] = words s
        readF "nan" = 0/0
        readF s = read s
