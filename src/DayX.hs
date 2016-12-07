{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections #-}

module DayX where

import ClassyPrelude

import Control.Monad.State (State, evalState, get, put, modify)
import Control.Monad.Trans.Either
import Data.Attoparsec.Text
       (Parser, parseOnly, many1, sepBy, char, letter, string, endOfLine)
import Data.Char (chr,ord)
import Data.List (transpose)
import Data.Vector ((//))
import Lens.Micro.Platform

import Debug.Trace

(!) :: IsSequence seq => seq -> Index seq -> Element seq
(!) = indexEx

input :: IO Text
input = readFile "data/dayX.txt"

parser = letter

result1 =
  runEitherT $
  do i <- EitherT (parseOnly parser <$> input)
     pure (i)
