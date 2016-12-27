{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections, GADTs #-}

module Day21 where

import AdventPrelude
import qualified Data.Sequence as S

input :: IO Text
-- input = readFile "data/day21.txt"
input = pure
  "swap position 4 with position 0\n\
  \swap letter d with letter b\n\
  \reverse positions 0 through 4\n\
  \rotate left 1 step\n\
  \move position 1 to position 4\n\
  \move position 3 to position 0\n\
  \rotate based on position of letter b\n\
  \rotate based on position of letter d"

data Op
  = SwapPos Int Int
  | SwapLetter Char Char
  | Rotate Int
  | RotateLetter Char
  | Reverse Int Int
  | Move Int Int
  deriving (Show)

parser =
  SwapPos <$> ("swap position " *> decimal)
          <*> (" with position " *> decimal) <|>
  SwapLetter <$> ("swap letter " *> letter)
             <*> (" with letter " *> letter) <|>
  Rotate <$> ("rotate " *> lr) <* (" step" *> many "s") <|>
  Reverse <$> ("reverse positions " *> decimal)
          <*> (" through " *> decimal) <|>
  RotateLetter <$> ("rotate based on position of letter " *> letter) <|>
  Move <$> ("move position " *> decimal)
       <*> (" to position " *> decimal)
  where lr = "left " *> decimal <|>
             "right " *> (neg <$> decimal)
        neg x = - x

  -- SwapPos Int Int
  -- SwapLetter Char Char
  -- Rotate Int
  -- RotateLetter Char
  -- Reverse Int Int
  -- Move Int Int

update :: Seq Char -> Op -> Seq Char
update cs (SwapPos ix iy) =
  let x = cs `S.index` ix
      y = cs `S.index` iy
  in S.update ix y (S.update iy x cs)
update cs (SwapLetter x y) =
  let (Just ix) = S.elemIndexL x cs
      (Just iy) = S.elemIndexL y cs
  in S.update ix y (S.update iy x cs)
-- update cs (Rotate n)
-- update cs (RotateLetter l)
update cs (Reverse a b) =
  let (h, t) = S.splitAt (b + 1) cs
      (h', m) = S.splitAt a h
  in traceShow (h,t,h',m) $
     h' >< (S.reverse m) >< t

result1 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy` endOfLine) <$> input)
     let s = "abcde"
     pure (foldl' update s $ take 4 i)
