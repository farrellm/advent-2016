{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day2 where

import ClassyPrelude

import Control.Lens
import Control.Monad.State (StateT, evalStateT, get, modify)
import Data.Attoparsec.Text hiding (D)
import Data.List (scanl)
import Data.Vector ((!))

import Debug.Trace

input :: IO Text
input = readFile "data/day2.txt"

test = "ULL\n\
       \RRDDD\n\
       \LURDL\n\
       \UUUUD"

data F3 = E1 | E2 | E3
  deriving (Show, Eq, Ord, Enum)

next :: F3 -> F3
next E1 = E2
next E2 = E3
next E3 = E3

prev :: F3 -> F3
prev E1 = E1
prev E2 = E1
prev E3 = E2

data Direction = U | D | L | R
  deriving (Show)

direction :: Parser Direction
direction =
  char 'U' *> pure U <|> char 'D' *> pure D <|>
  char 'L' *> pure L <|> char 'R' *> pure R

directionSet :: Parser [[Direction]]
directionSet = many1 direction `sepBy1` endOfLine

move1 :: (F3, F3) -> Direction -> (F3, F3)
move1 (r, c) d = case d of
  U -> (prev r, c)
  D -> (next r, c)
  L -> (r, prev c)
  R -> (r, next c)

move :: (F3, F3) -> [Direction] -> (F3, F3)
move = foldl' move1

button :: (F3, F3) -> Int
button (r, c) = fromEnum r * 3 + fromEnum c + 1

positions :: [[Direction]] -> [(F3, F3)]
positions ds = scanl move origin ds

buttons :: [[Direction]] -> [Char]
buttons ds = concat . map (show . button) $ positions ds

origin :: (F3, F3)
origin = (E2, E2)

result1 =
  do t <- input
     -- t <- pure test :: IO Text
     case parseOnly directionSet t of
       Left err -> pure err
       Right ds -> let _:bs = buttons ds
                   in pure bs

pad :: Vector (Vector Char)
pad = fromList [fromList "0000000"
               ,fromList "0001000"
               ,fromList "0023400"
               ,fromList "0567890"
               ,fromList "00ABC00"
               ,fromList "000D000"
               ,fromList "0000000"]

button' :: (Int, Int) -> Char
button' (r, c) = pad ! r ! c

move1' :: (Int, Int) -> Direction -> (Int, Int)
move1' x d =
  let x' = mv x d
  in if button' x' /= '0'
     then x'
     else x
  where mv (r, c) d = case d of
          U -> (r-1, c)
          D -> (r+1, c)
          L -> (r, c-1)
          R -> (r, c+1)

move' :: (Int, Int) -> [Direction] -> (Int, Int)
move' = foldl' move1'

origin' :: (Int, Int)
origin' = (3, 1)

positions' :: [[Direction]] -> [(Int, Int)]
positions' ds = scanl move' origin' ds

buttons' :: [[Direction]] -> [Char]
buttons' ds = map button' $ positions' ds

result2 =
  do t <- input
     -- t <- pure test :: IO Text
     case parseOnly directionSet t of
       Left err -> pure err
       Right ds -> let _:bs = buttons' ds
                   in pure bs
