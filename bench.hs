{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main
import Data.Float.BinString as F

main = defaultMain [
        bgroup "read" [
         bench "2" $ whnf F.readFloat "0x1.23p2",
         bench "3" $ whnf F.readFloat "0x1.2342123121299821p100"
        ],

        bgroup "show" [
         bench "x" $ whnf F.showFloat pi
        ]
       ]

