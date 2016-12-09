{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections #-}

module Day9 where

import ClassyPrelude

import Data.Attoparsec.Text
       (Parser, IResult(..), parseOnly, parse, sepBy, anyChar, char, decimal)
import Data.List (elemIndex)

input :: IO Text
input = readFile "data/day9.txt"

-- Note: order matters! Left always succeeds, so must check Right first!
comp :: Parser (Either Char (Int, Int))
comp = Right <$> ((,) <$> (char '(' *> decimal) <*> (char 'x' *> decimal) <* char ')') <|>
       Left <$> anyChar


process :: Text -> Text
process "" = ""
process "\n" = ""
process t =
  case parse comp t of
    Done rs (Right (i,n)) ->
      let (a,b) = splitAt i rs
      in concat (replicate n a :: [Text]) <> process b
    Done rs (Left c) -> fromString [c] <> process rs
    _ -> error "parse error"

result1 = (length . process) <$> input


process' :: Text -> Int
process' "" = 0
process' "\n" = 0
process' t =
  case parse comp t of
    Done rs (Right (i,n)) ->
      let (a,b) = splitAt i rs
          a' = process' a
      in n * a' + process' b
    Done rs (Left _) -> 1 + process' rs
    _ -> error "parse error"

result2 = process' <$> input
