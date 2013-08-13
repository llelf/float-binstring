module Data.Float.BinString.Test where

import Data.Float.BinString
import Test.QuickCheck
import Data.Maybe

rev_prop x = Just x == readFl (showFl x)

