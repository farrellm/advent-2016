{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections, GADTs #-}

module DayX where

import AdventPrelude2

input :: IO Text
input = readFile "data/dayX.txt"

parser = letter

result1 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy` endOfLine) <$> input)
     pure (take 5 i)
