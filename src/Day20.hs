{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections, GADTs #-}

module Day20 where

import AdventPrelude
import qualified Data.Sequence as S

input :: IO Text
input = readFile "data/day20.txt"

parser = (,) <$> (decimal <* char '-') <*> decimal

findMin :: Int -> Seq (Int, Int) -> Int
findMin a ps =
  traceShow (a, S.length ps) $
  case viewl ps of
    EmptyL -> a
    _ ->
      let is = S.findIndicesL (inside a) ps
          a' = foldl' firstOutside a (fmap (S.index ps) is)
      in if null is
           then a
           else findMin a' (foldl' (flip S.deleteAt) ps is)
  where
    inside x (l, u) = l <= x && x <= u
    firstOutside x (_, u) = max x (u + 1)

result1 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy1` endOfLine) <$> input)
     pure (findMin 0 (S.fromList i))

countAllowed :: Word32 -> Seq (Word32, Word32) -> Word32
countAllowed a ps =
  -- traceShow (a, S.length ps) $
  -- traceShow (a, ps) $
  case viewl ps of
    EmptyL -> maxBound - a + 1
    _ ->
      let is = S.findIndicesL (inside a) ps
          a' = foldl' firstOutside a (fmap (S.index ps) is)
          Just m = minimum <$> (fromNullable $ fmap fst ps)
      in if null is
           then -- traceShow (0, m, a, m-a) $
                (m - a) + countAllowed m ps
           else -- traceShow (1, a') $
                countAllowed a' (foldl' (flip S.deleteAt) ps is)
  where
    inside x (l, u) = l <= x && x <= u
    firstOutside x (_, u) = max x (u + 1)

result2 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy1` endOfLine) <$> input)
     let i' = fmap (bimap fromIntegral fromIntegral) i
     pure (countAllowed 0 (S.fromList i'))
