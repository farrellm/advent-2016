{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day1 where

import ClassyPrelude

import Control.Lens
import Control.Monad.Trans.Either
import Control.Monad.State (StateT, evalStateT, get, modify)
import Data.Attoparsec.Text
import Linear

import Debug.Trace

test1 :: Text
test1 = "R2, L3"

test2 :: Text
test2 = "R2, R2, R2"

test3 :: Text
test3 = "R5, L5, R5, R3"

test4 :: Text
test4 = "R8, R4, R4, R8"

input :: IO Text
input = readFile "data/day1.txt"

l :: M44 Int
l = V4 (V4 1 0 0 0)
       (V4 0 1 0 0)
       (V4 0 0 0 (-1))
       (V4 0 0 1 0)

r :: M44 Int
r = V4 (V4 1 0 0 0)
       (V4 0 1 0 0)
       (V4 0 0 0 1)
       (V4 0 0 (-1) 0)

m :: M44 Int
m = V4 (V4 0 0 1 0)
       (V4 0 0 0 1)
       (V4 0 0 0 0)
       (V4 0 0 0 0)

origin :: V4 Int
origin = V4 0 0 0 1

type Move = Either Int Int

update :: V4 Int -> Move -> V4 Int
update x d = case d of
  Right i -> (identity !+! (i *!! m)) !*! r !* x
  Left i -> (identity !+! (i *!! m)) !*! l !* x

move :: Parser Move
move =
  string "R" *> (Right <$> decimal) <|>
  string "L" *> (Left <$> decimal)

moves :: Parser [Move]
moves = move `sepBy` string ", "

result1 =
  runEitherT $
  do ms <- EitherT (parseOnly moves <$> input)
     let v = foldl' update origin ms
     pure $ abs (v ^. _x) + abs (v ^. _y)

data Step = L | R | S

expand :: Move -> [Step]
expand (Left i) = L : replicate i S
expand (Right i) = R : replicate i S

step :: V4 Int -> Step -> V4 Int
step v s = case s of
  L -> l !* v
  R -> r !* v
  S -> (identity !+! m) !* v

result2 =
  runEitherT $
  do ms <- EitherT (parseOnly moves <$> input)
     -- ms <- EitherT (parseOnly moves <$> (pure test4 :: IO Text))
     let ss = ms >>= expand
         ev = evalStateT (foldM go origin ss) mempty
     pure $
       case ev of
         Left v -> Left (abs (v ^. _x) + abs (v ^. _y))
         Right v -> Right v
  where go
          :: V4 Int -> Step -> StateT (Set (V2 Int)) (Either (V2 Int)) (V4 Int)
        go v S =
          do let v' = step v S
                 xy = v' ^. _xy
             s <- get
             if xy `member` s
                then lift (Left xy)
                else do modify (insertSet xy)
                        pure v'
        go v r = pure (step v r)
