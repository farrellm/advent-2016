{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections, GADTs, FlexibleContexts #-}

module Day22 where

import AdventPrelude
import qualified Data.Sequence as S

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

step :: [((Int, Int), Grid)] -> State (Int, Int, Set Grid) ()
step ps = do
  s <- use _1
  minD <- use _2
  past <- use _3
  traceShow (s, minD, length ps) $
   if any isDone ps
    then pure ()
    else do
      _1 %= succ
      let ps' = do
            ((x, y), (o, t)) <- ps
            let lx = S.length o
                ly = S.length (S.index o 0)
                isViable (x1, y1) (x2, y2) =
                  o !! x1 !! y1 > 0 &&
                  0 <= x2 && x2 < lx &&
                  0 <= y2 && y2 < ly &&
                  o !! x1 !! y1 <= (t !! x2 !! y2) - (o !! x2 !! y2)
            x1 <- [0 .. lx - 1]
            y1 <- [0 .. ly - 1]
            (x2, y2) <- [(x1+1,y1), (x1-1,y1), (x1,y1+1), (x1,y1-1)]
            guard (isViable (x1,y1) (x2,y2))
            let (x', y') =
                  if x == x1 && y == y1
                     then (x2, y2)
                     else (x, y)
                o' = o & ix x1 . ix y1 .~ 0
                       & ix x2 . ix y2 %~ (+ o !! x1 !! y1)
            guard (notMember (o', t) past)
            guard (x' + y' < minD + 1)
            pure ((x', y'), (o', t))
          ds = fmap (uncurry (+) . fst) ps'
      _2 .= minimum (minD `ncons` ds)
      step ps'
  where
    isDone ((x, y), _)
      | x == 0 && y == 0 = True
    isDone _ = False

result2 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy1` endOfLine) <$> input)
     let tgtX = maximumEx (fmap nX $ filter ((== 0) . nY) i)
         g = mkGrid i
     pure (execState (step [((tgtX, 0), g)]) (0, tgtX, mempty))
     -- pure (mkGrid i)
     -- pure i
