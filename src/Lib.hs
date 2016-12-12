{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}
module Lib
    ( someFunc
    ) where

import ClassyPrelude

import qualified Day11

someFunc :: IO ()
someFunc = Day11.result2 >>= print
