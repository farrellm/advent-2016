{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections, GADTs #-}

module Day13 where

import AdventPrelude
import qualified Data.HashSet as S

type Coord = (Int, Int)

isOpen :: Int -> Int -> Bool
isOpen x y =
  let n = x*x + 3*x + 2*x*y + y + y*y + 1350 -- 10
      c = popCount n
  in c `mod` 2 == 0

move :: Coord -> [Coord]
move (x, y) = do
  (x', y') <- [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
  guard (x'>=0 && y'>=0 && isOpen x' y')
  pure (x', y')

loop :: Coord -> (Int, [Coord]) -> State (HashSet Coord) [Int]
loop tgt (i, cs) = do
  old <- get
  let (a, b) = partition (== tgt) cs
      cs' = concatMap move b
      cs'' = filter (`notMember` old) cs'
  put (old <> setFromList cs'')
  next <- loop tgt (i+1, cs'')
  traceShow (i, length cs) $
    pure (map (const i) a <> next)

result1 =
  let -- tgt = (7,4)
      tgt = (31,39)
  in (take 1 $ evalState (loop tgt (0, [(1,1)])) S.empty)


loop' :: Int -> (Int, [Coord]) -> State (HashSet Coord) ()
loop' tgt (i, cs)
  | i == tgt = pure ()
  | otherwise = do
      old <- get
      let cs' = concatMap move cs
          cs'' = filter (`notMember` old) cs'
      put (old <> setFromList cs'')
      loop' tgt (i+1, cs'')

result2 =
  S.size $ execState (loop' 50 (0, [(1,1)])) (setFromList [(1,1)])
