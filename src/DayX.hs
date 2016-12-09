{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections #-}

module DayX where

import ClassyPrelude

import Control.Monad.State (State, evalState, evalStateT, get, put, modify)
import Control.Monad.Trans.Either
import Data.Attoparsec.Text
       (Parser, parseOnly, many1, sepBy, char, digit, letter, space,
        string, skipSpace, decimal, hexadecimal, double, signed, endOfLine)
import Data.Char (chr,ord)
import Data.List (elemIndex, transpose)
import Data.Vector ((//))
import Lens.Micro.Platform

(!) :: IsSequence seq => seq -> Index seq -> Element seq
(!) = indexEx

input :: IO Text
input = readFile "data/dayX.txt"

parser = letter

result1 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy` endOfLine) <$> input)
     pure (take 5 i)
