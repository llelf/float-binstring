module Data.Float.Test where

import Data.Float.BinString
import Test.QuickCheck
import Data.Maybe

rev x = x == (fromJust . readFl . showFl $ x)

