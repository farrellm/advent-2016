{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections, GADTs #-}

module DayX where

import AdventPrelude
import Data.List (iterate)

input :: String
input = "......^.^^.....^^^^^^^^^...^.^..^^.^^^..^.^..^.^^^.^^^^..^^.^.^.....^^^^^..^..^^^..^^.^.^..^^..^^^.."
-- input = "..^^."
-- input = ".^^.^.^^^^"

showRow :: [Bool] -> String
showRow = fmap showCell
  where showCell c | c = '^'
                   | otherwise  = '.'

nextRow :: [Bool] -> [Bool]
nextRow = nextCell . (False :) . (<> [False])
  where nextCell (l: rs@(c:r:_)) =
          let n = (l && c && not r) || (c && r && not l) ||
                  (l && not c && not r) || (r && not c && not l)
          in n : nextCell rs
        nextCell _ = []

result1 =
  let r0 = fmap (== '^') input
      rs = take 40 (iterate nextRow r0)
  in sum . fmap ((1 -) . fromEnum) $ concat rs

result2 =
  let r0 = fmap (== '^') input
      rs = take 400000 (iterate nextRow r0)
  in sum . fmap ((1 -) . fromEnum) $ concat rs
