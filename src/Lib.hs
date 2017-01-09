{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import ClassyPrelude

import qualified Day23

someFunc :: IO ()
someFunc = Day23.result2 >>= print
