{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}

module Day24 where

import AdventPrelude2
import Control.Arrow
import Data.List (minimum)
import Data.String (lines)
import Data.Vector (Vector, (!), (!?))
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

input :: IO Text
input = readFile "data/day24.txt"
-- input = pure
--   "###########\n\
--   \#0.1.....2#\n\
--   \#.#######.#\n\
--   \#4.......3#\n\
--   \###########"

type Label = Char
type Coord = (Int, Int)
type Metric = Label -> Label -> Int
type Grid = Vector (Vector Bool)
type MazeMap = Vector (Vector Char)

findGoals :: [[Char]]
          -> (Coord, Map Label Coord)
findGoals vs =
  let ((_, start):gls) = sort $ go vs
      goals = M.fromList gls
  in (start, goals)
  where
    go vs =
      concatMap goLine (zip [0..] vs)
    goLine (r, v) =
      concatMap (goCell r) (zip [0..] v)
    goCell r (c, q)
      | q == '#' || q == '.' = []
      | otherwise = [(q, (r, c))]

coordDist :: Coord -> Coord -> Int
coordDist (ar, ac) (br, bc) = abs (ar - br) + abs (ac - bc)

goalDist :: Map Label Coord -> Metric
goalDist goals a b =
  let Just ac = M.lookup a goals
      Just bc = M.lookup b goals
  in coordDist ac bc

data PathNode = PathNode
  { _pathLength :: Int
  , _next :: Map Label PathNode
  }

remainingLength :: PathNode -> Label -> Set Label -> Int
remainingLength pathCache f rs =
  let Just l = lookupLength (f : S.toAscList (S.delete f rs)) pathCache
  in l
  where
    lookupLength ls (PathNode d ns) =
      case ls of
        [] -> pure d
        l:lss -> lookupLength lss =<< M.lookup l ns

findLength :: Metric -> PathNode -> Label -> Set Label -> Int
findLength dist pathCache f rs
  | S.null rs = 0
  | otherwise =
    -- traceShow ("findLength", f, rs) $
    minimum $
    do s <- S.toList rs
       let d = dist f s
           -- l = findLength goals s (S.delete s rs)
           l = remainingLength pathCache s (S.delete s rs)
       pure (d + l)

fillCache :: Map Label Coord -> PathNode -> PathNode
fillCache goals pathCache =
  let ls = M.keys goals
      go f rs =
        PathNode
          (findLength (goalDist goals) pathCache f (S.fromList rs))
          (M.fromList $ map (\l -> (l, go f (l : rs))) ls)
  in PathNode 0 (M.fromList $ map (\l -> (l, go l [])) ls)

data MazeState = MazeState
  { _position :: Coord
  , _remaining :: Set Label
  }
  deriving (Show, Eq, Ord)

opens :: Grid -> Coord -> [Coord]
opens g (r, c) = do
  n@(nr, nc) <- [(r+1, c), (r-1, c), (r, c+1), (r, c-1)]
  guard (g ! nr ! nc)
  pure n

moves :: MazeMap -> Grid -> MazeState -> [MazeState]
moves m g MazeState {..} = do
  n <- opens g _position
  let (r, c) = n
      q = m ! r ! c
  [MazeState n (S.delete q _remaining)]

isDone :: MazeState -> Bool
isDone MazeState {_remaining = r} = S.null r

minRemaining :: Map Label Coord -> PathNode -> MazeState -> Int
minRemaining gs pathCache MazeState{..} =
  case S.toList _remaining of
    [] -> 0
    fs -> minimum $ do
      f <- fs
      let rs = S.delete f _remaining
          Just g = M.lookup f gs
      [coordDist _position g + remainingLength pathCache f rs]

result1 = do
  ls <- lines . toS <$> input
  let (start, gs) = findGoals ls
      m = V.fromList (map V.fromList ls)
      g = map (map (/= '#')) m
      labels = M.keys gs
      pathCache = fillCache gs pathCache
      mazeStart =
        MazeState
        { _position = start
        , _remaining = S.fromList labels
        }
      ms = moves m g
      heur = minRemaining gs pathCache
  -- pure (start, gs, opens g start)
  -- pure (moves m g mazeStart)
  pure ((pred . length) $ stepSearch ms heur isDone identity mazeStart)

isDone' :: Coord -> MazeState -> Bool
isDone' (or, oc) m@MazeState {_position = (r, c)} =
  r == or && c == oc && isDone m

result2 = do
  ls <- lines . toS <$> input
  let (start, gs) = findGoals ls
      m = V.fromList (map V.fromList ls)
      g = map (map (/= '#')) m
      labels = M.keys gs
      pathCache = fillCache gs pathCache
      mazeStart =
        MazeState
        { _position = start
        , _remaining = S.fromList labels
        }
      ms = moves m g
      heur = minRemaining gs pathCache
  pure ((pred . length) $ stepSearch ms heur (isDone' start) identity mazeStart)
