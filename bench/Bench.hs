-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main
import Criterion.Config
import Data.Int
import System.Logger.Message

import qualified Data.ByteString.Lazy as L

main :: IO ()
main = defaultMainWith defaultConfig (return ())
    [ bgroup "direct"
        [ bench "msg/8"  (whnf (f False) 8)
        , bench "msg/16" (whnf (f False) 16)
        , bench "msg/32" (whnf (f False) 32)
        ]
    , bgroup "netstr"
        [ bench "msg/8"  (whnf (f True) 8)
        , bench "msg/16" (whnf (f True) 16)
        , bench "msg/32" (whnf (f True) 32)
        ]
    , bgroup "direct"
        [ bench "field/8"  (whnf (g False) 8)
        , bench "field/16" (whnf (g False) 16)
        , bench "field/32" (whnf (g False) 32)
        ]
    , bgroup "netstr"
        [ bench "field/8"  (whnf (g True) 8)
        , bench "field/16" (whnf (g True) 16)
        , bench "field/32" (whnf (g True) 32)
        ]
    ]

f :: Bool -> Int -> Int64
f b n = L.length
      . render ", " b
      . foldr1 (.)
      . replicate n
      $ msg (val "hello world")

g :: Bool -> Int -> Int64
g b n = L.length
      . render ", " b
      . foldr1 (.)
      . replicate n
      $ field "key" (val "value")
