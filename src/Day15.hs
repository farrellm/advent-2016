{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections, GADTs #-}

module Day15 where

import AdventPrelude
import qualified Data.Map as M

input :: IO Text
input = readFile "data/day15.txt"

data Disc = Disc
  { num :: Int
  , nPos :: Int
  , curPos :: Int
  } deriving (Show)

data Capsule = Capsule
  { start :: Int
  , curDisc :: Int
  } deriving (Show)

parser :: Parser Disc
parser =
  Disc <$> (string "Disc #" *> decimal) <*>
  (string " has " *> decimal) <*>
  (string " positions; at time=0, it is at position " *> decimal) <*
  char '.'

rotateDisc :: Disc -> Disc
rotateDisc d = d{ curPos = (curPos d + 1) `mod` nPos d }

dropCapsule :: Capsule -> Capsule
dropCapsule c = c{ curDisc = curDisc c + 1 }

isOpen :: Disc -> Bool
isOpen d = curPos d == 0

step :: (Int, [Capsule]) -> State (Map Int Disc) [Capsule]
step (i, cs) = do
  ds <- get
  let cs' = do
        c <- Capsule i 0 : cs
        guard (isOpen (ds ! curDisc c))
        pure (dropCapsule c)
  modify (map rotateDisc)
  let (a, b) = partition ((== M.size ds) . curDisc) cs'
  (a <>) <$> step (i+1, b)


result1 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy1` endOfLine) <$> input)
     let discs = mapFromList ((num &&& id) <$> (Disc 0 1 0 : i))
     pure (take 1 (evalState (step (0, [])) discs))

result2 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy1` endOfLine) <$> input)
     let discs = mapFromList
                 ((num &&& id) <$>
                  (Disc 0 1 0 : Disc 7 11 0 : i))
     traceM (show discs)
     pure (take 1 (evalState (step (0, [])) discs))
