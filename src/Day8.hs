{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections #-}

module Day8 where

import ClassyPrelude

import Control.Monad.Trans.Either
import Data.Attoparsec.Text
       (Parser, parseOnly, sepBy, char, letter, string, decimal, endOfLine)
import Data.List (transpose)

input :: IO Text
input = readFile "data/day8.txt"

type Screen = [String]

screen :: Screen
screen = replicate 6 (replicate 50 '.')

data Cmd = Rect Int Int | RotateY Int Int | RotateX Int Int
  deriving (Show)

parser =
  (Rect <$> (string "rect " *> decimal <* char 'x') <*> decimal) <|>
  (RotateY <$> (string "rotate row y=" *> decimal <* " by ") <*> decimal) <|>
  (RotateX <$> (string "rotate column x=" *> decimal <* " by ") <*> decimal)

update :: Screen -> Cmd -> Screen
update s (Rect w h) =
  let (a, b) = splitAt h s
      a' = map (line w) a
  in a' <> b
  where line :: Int -> String -> String
        line w a =
          let y = drop w a
          in replicate w '#' <> y

update s (RotateY r n) =
  let (a,(x:b)) = splitAt r s
      (xh,xt) = splitAt ((length x) - n) x
      x' = xt <> xh
  in a <> (x' : b)

update s (RotateX c n) =
  transpose (update (transpose s) (RotateY c n))

result1 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy` endOfLine) <$> input)
     pure (length .
           filter (== '#') .
           concat $
           foldl' update screen i)

result2 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy` endOfLine) <$> input)
     let r = (foldl' update screen i)
     liftIO (putStrLn (pack $ unlines r))
