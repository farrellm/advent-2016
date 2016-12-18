{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections, GADTs #-}

module Day17 where

import AdventPrelude

import qualified Crypto.Hash.MD5 as MD5

input :: String
-- input = "qzthpkfp"
-- input = "ihgpwlah"
-- input = "kglvqrro"
-- input = "ulqzkmiv"
input = "qzthpkfp"

data Move = U | D | L | R
  deriving (Show)

type Path = [Move]
type Coord = (Int, Int)

move :: Coord -> Move -> Coord
move (x, y) m = case m of
  U -> (x, y-1)
  D -> (x, y+1)
  L -> (x-1, y)
  R -> (x+1, y)

validCoord :: Coord -> Bool
validCoord (x, y) = 0 <= x && x < 4 && 0 <= y && y < 4

validMoves :: String -> Path -> [Move]
validMoves i ms =
  let j = i <> concat (fmap show ms) :: String
      h = MD5.hash (fromString j)
      bs = take 4 (fmap isOpen . unpack $ showBytes h)
  in fmap fst $ filter snd (zip [U,D,L,R] bs)
  where isOpen c = c > 'a'

step :: String -> [(Coord, Path)] -> [String]
step _ [] = []
step i xs =
  let xs' = do
        (c, p) <- xs
        m <- validMoves i p
        let c' = move c m
            p' = p <> [m]
        guard (validCoord c')
        pure (c', p')
      (a, b) = partition ((== (3, 3)) . fst) xs'
  in fmap (concat . fmap show .
           -- (\i -> traceShow i i) .
           snd) a <> step i b

result1 =
  take 1 (step input [((0, 0), [])])

result2 =
  lastEx $ fmap length (step input [((0, 0), [])])
