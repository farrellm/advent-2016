{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections #-}

module Day6 where

import ClassyPrelude

import Lens.Micro.Platform
import Data.Attoparsec.Text hiding (take)
import Data.List (transpose)

import Debug.Trace

input :: IO String
input = readFile "data/day6.txt"

result1 =
  do ls <- lines <$> input
     pure . map (snd . headEx . sort . map toPair . groupAll) $ transpose ls
  where toPair xs@(x:_) = (-(length xs),x)

result2 =
  do ls <- lines <$> input
     pure . map (snd . headEx . sort . map toPair . groupAll) $ transpose ls
  where toPair xs@(x:_) = ((length xs),x)
