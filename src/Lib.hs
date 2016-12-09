{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}
module Lib
    ( someFunc
    ) where

import ClassyPrelude

import qualified Day5 as Day5
import qualified Day9 as Day9

day5 = Day5.y
day9 = Day9.result2

someFunc :: IO ()
someFunc = day9 >>= print
