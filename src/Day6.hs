{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections #-}

module Day6 where

import ClassyPrelude

import Lens.Micro.Platform
import Data.Attoparsec.Text hiding (take)
import Data.List (transpose)

import Debug.Trace

input :: IO Text
input = readFile "data/day6.txt"

result1 = do
  mls <- parseOnly (many letter `sepBy` endOfLine) <$> input
  pure $ case mls of
    Right ls -> Right $ map ((^._2) . headEx . sort . map toPair . groupAll) $ transpose ls
    Left x -> Left x
  where toPair xs@(x:_) = (-(length xs), x)

result2 = do
  mls <- parseOnly (many letter `sepBy` endOfLine) <$> input
  pure $ case mls of
    Right ls -> Right $ map ((^._2) . headEx . sort . map toPair . groupAll) $ transpose ls
    Left x -> Left x
  where toPair xs@(x:_) = ((length xs), x)
