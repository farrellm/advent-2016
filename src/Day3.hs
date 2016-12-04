{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day3 where

import ClassyPrelude

import Control.Lens
import Control.Monad.State (StateT, evalStateT, get, modify)
import Data.Attoparsec.Text hiding (take)
import Data.List (transpose)

import Debug.Trace

input :: IO Text
input = readFile "data/day3.txt"

nums :: Parser [Int]
nums = many decimal

data Trip = Trip Int (Int, Int)
  deriving (Show)

mkTrip a b c
  | a >= b && a >= c = Trip a (b,c)
  | b >= a && b >= c = Trip b (a,c)
  | c >= a && c >= b = Trip c (a,b)

trip :: Parser Trip
trip =
  mkTrip <$> (skipSpace *> decimal) <*> (skipSpace *> decimal) <*> (skipSpace *> decimal)

isTriangle :: Trip -> Bool
isTriangle (Trip a (b,c)) = a < (b+c)

result1 =
  do els <- parseOnly (trip `sepBy1` endOfLine) <$> input
     pure ((length . filter isTriangle) <$> els)

row :: Parser [Int]
row =
  l3 <$> (skipSpace *> decimal) <*> (skipSpace *> decimal) <*> (skipSpace *> decimal)
  where l3 a b c = [a, b, c]

result2 =
  do ers <- parseOnly (row `sepBy1` endOfLine) <$> input
     let els = (map listToTrip . concat . map (chunksOf 3) . transpose) <$> ers
     pure (length <$> filter isTriangle <$> els)
  where chunksOf _ [] = []
        chunksOf n ls = let (h,t) = splitAt n ls
                        in h : chunksOf n t
        listToTrip [a, b, c] = mkTrip a b c
