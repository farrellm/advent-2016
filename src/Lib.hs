{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import ClassyPrelude

import qualified Day22

someFunc :: IO ()
someFunc = Day22.result2 >>= print
