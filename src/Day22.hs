{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections, GADTs, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Day22 where

import AdventPrelude
import Control.Arrow (returnA)
import qualified Data.Sequence as S
import Data.Ratio ((%))
import Data.Text.Format
import Data.Set (size)

input :: IO Text
input = readFile "data/day22.txt"

-- input = pure "/dev/grid/node-x0-y0   10T    8T     2T   80%\n\
--              \/dev/grid/node-x0-y1   11T    6T     5T   54%\n\
--              \/dev/grid/node-x0-y2   32T   28T     4T   87%\n\
--              \/dev/grid/node-x1-y0    9T    7T     2T   77%\n\
--              \/dev/grid/node-x1-y1    8T    0T     8T    0%\n\
--              \/dev/grid/node-x1-y2   11T    7T     4T   63%\n\
--              \/dev/grid/node-x2-y0   10T    6T     4T   60%\n\
--              \/dev/grid/node-x2-y1    9T    8T     1T   88%\n\
--              \/dev/grid/node-x2-y2    9T    6T     3T   66%"

data Node = Node
  { nX :: Int
  , nY :: Int
  , nUsed :: Int
  , nAvail :: Int
  } deriving (Show, Eq)

parser =
  Node <$> ("/dev/grid/node-x" *> decimal)
       <*> ("-y" *> decimal)
       <* (skipSpace *> decimal <* "T")
       <*> (skipSpace *> decimal <* "T")
       <*> (skipSpace *> decimal <* "T")
       <* (skipSpace *> decimal <* "%")

isViable :: Node -> Node -> Bool
isViable n1 n2 =
  nUsed n1 > 0 && n1 /= n2 && nUsed n1 <= nAvail n2

enumViable :: [Node] -> [(Node,Node)]
enumViable ns = do
  a <- ns
  b <- ns
  guard (isViable a b)
  pure (a,b)

result1 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy1` endOfLine) <$> input)
     pure (length $ enumViable i)

type Grid = (Seq (Seq Int), Seq (Seq Int))
type Coord = (Int, Int)

mkGrid :: [Node] -> Grid
mkGrid ns =
  let maxX = maximumEx (fmap nX ns)
      maxY = maximumEx (fmap nY ns)
      col = S.replicate (maxY + 1) 0
      gridZero = S.replicate (maxX + 1) col
  in -- traceShow (maxX, maxY, col, gridZero) $
     foldl' setCell (gridZero, gridZero) ns
  where setCell (u, s) n =
          (u & ix (nX n) . ix (nY n) .~ nUsed n,
           s & ix (nX n) . ix (nY n) .~ nUsed n + nAvail n)

tr :: Seq (Seq a) -> Seq (Seq a)
tr ss@(viewl -> (viewl -> _ :< _) :< _) =
  let hs = concatMap (S.take 1) ss
      rs = map (S.drop 1) ss
  in hs S.<| tr rs
tr _ = S.empty

showGrid :: Grid -> LText
showGrid (go, gt) =
  let l = fromIntegral . S.length $ headEx go
      s = "\n" <> replicate (l * 5 + (l - 1) * 3) '-' <> "\n"
  in intercalate s $ map showRow (S.zip (tr go) (tr gt))
  where
    showRow (ro, rt) = intercalate " | " $ map showCell (zip ro rt)
    showCell (o, t) = format "{}/{}" (left 2 ' ' (tshow o), left 2 ' ' (tshow t))

showGrid' :: (Coord, Coord, Grid) -> LText
showGrid' ((x, y), _, (go, gt)) =
  unlines $ S.mapWithIndex showRow (S.zip (tr go) (tr gt))
  where
    showRow y' (ro, rt) = unwords $ S.mapWithIndex (showCell y') (zip ro rt)
    showCell _ _ (0, t) = "_"
    showCell y' x' (o, t)
      | x == x' && y == y' = "G"
      | o > 100 = "#"
      | otherwise = "."

showResult :: [(Coord, Coord, Grid)] -> LText
showResult rs =
  intercalate "\n\n" (map showGrid' rs)

used x y = _1 . ix x . ix y
total x y = _2 . ix x . ix y

result2 :: IO (Either String Int)
result2 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy1` endOfLine) <$> input)
     let tgtX = maximumEx (fmap nX $ filter ((== 0) . nY) i)
         Just (zx, zy) = (nX &&& nY) <$> (find ((== 0) . nUsed) i)
         g = mkGrid i
         r = stepSearch (step g) heur isDone id ((tgtX, 0), (zx, zy))
     pure (length r - 1)
  where
    isDone ((x, y), _) = x == 0 && y == 0
    heur ((x, y), (zx, zy)) = x + y + abs (x - zx) + abs (y - zy)
    step g (xy@(x, y), zxy@(zx, zy)) = do
      let mx = S.length (g ^. _1)
          my = S.length (g ^. _1 . ix 0)
          isValid x2 y2 =
            let Just u = g ^? used x2 y2
            in 0 <= x2 && x2 < mx && 0 <= y2 && y2 < my && u < 100
      zxy'@(zx', zy') <-
        [(zx + 1, zy), (zx - 1, zy), (zx, zy + 1), (zx, zy - 1)]
      guard (isValid zx' zy')
      let xy' =
            if xy == zxy'
              then zxy
              else xy
      pure (xy', zxy')
