{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections #-}

module DayX where

import ClassyPrelude

import Lens.Micro.Platform
import Control.Monad.State (State, evalState, get, put, modify)
import Data.Attoparsec.Text hiding (take)
import Data.Vector ((//))

import Debug.Trace

(!) :: IsSequence seq => seq -> Index seq -> Element seq
(!) = indexEx

input :: IO Text
input = readFile "data/dayX.txt"
