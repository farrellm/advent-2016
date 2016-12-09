{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections #-}

module Day9 where

import ClassyPrelude

import Data.Attoparsec.Text
       (Parser, IResult(..), parseOnly, parse, sepBy, char, decimal)
import Data.List (elemIndex)

input :: IO Text
input = readFile "data/day9.txt"

comp :: Parser (Int, Int)
comp = (,) <$> (char '(' *> decimal) <*> (char 'x' *> decimal) <* char ')'

process :: Text -> Text
process "" = ""
process "\n" = ""
process t =
  case parse comp t of
    Done rs (i,n) ->
      let (a,b) = splitAt i rs
      in (concat (replicate n a :: [Text])) <> process b
    Partial _ -> error "partial parse, this should not happen!"
    Fail _ _ _ ->
      let (c,cs) = splitAt 1 t
      in c <> process cs

result1 = (length . process) <$> input


process' :: (Int, Int) -> Text -> Int
process' _ "" = 0
process' _ "\n" = 0
process' z@(0,0) t =
  case parse comp t of
    Done rs (i,n) ->
      let (a,b) = splitAt i rs
          a' = process' (0,0) a
      in n * a' + process' (0,0) b
    Partial _ -> error "partial parse, this should not happen!"
    Fail _ _ _ ->
      let (c,cs) = splitAt 1 t
      in 1 + process' z cs

result2 = process' (0,0) <$> input
