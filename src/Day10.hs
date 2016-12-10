{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections, GADTs #-}

module Day10 where

import ClassyPrelude

import Control.Monad.State (State, evalState, evalStateT, get, put, modify)
import Control.Monad.Trans.Either
import Data.Attoparsec.Text
       (Parser, parseOnly, many1, sepBy, char, digit, letter, space,
        string, skipSpace, decimal, hexadecimal, double, signed, endOfLine)
import Data.Char (chr,ord)
import Data.List (elemIndex, transpose)
import Data.Vector ((//))
import Lens.Micro.Platform

(!) :: IsSequence seq => seq -> Index seq -> Element seq
(!) = indexEx

input :: IO Text
input = readFile "data/day10.txt"

newtype Value = Value Int
  deriving (Show)
newtype Bot = Bot Int
  deriving (Show, Ord, Eq)

data Reciever = RBot Int | Output Int
  deriving (Show, Ord, Eq)

data Input = Init Value Bot | Gives Bot Reciever Reciever
  deriving (Show)

reciever =
  RBot <$> (string "bot " *> decimal) <|>
  Output <$> (string "output " *> decimal)

parser =
  Init <$> (Value <$> (string "value " *> decimal)) <*>
    (Bot <$> (string " goes to bot " *> decimal)) <|>
  Gives <$> (Bot <$> (string "bot " *> decimal)) <*>
    (string " gives low to " *> reciever) <*>
    (string " and high to " *> reciever)

type Bots = Map Reciever [Int]

initialize :: Bots -> Input -> Bots
initialize bs (Init (Value v) (Bot b)) =
  case lookup (RBot b) bs of
    Just vs -> insertMap (RBot b) (v:vs) bs
    Nothing -> insertMap (RBot b) [v] bs
initialize bs _ = bs

update :: [Input] -> Bots -> Either Reciever Bots
update i bs =
  let (b,vs) =
        case find (\(_,vs) -> length vs == 2)
                  (mapToList bs) of
          Just p -> p
          Nothing -> error "no bot with 2!"
      [l,h] = sort vs
  in traceShow (b,vs) $
     if (sort vs) == [17,61]
        then Left b
        else case find (bGives b) i of
               Just (Gives _ x y) -> pure (ac l x . ac h y $ insertMap b [] bs)
  where bGives (RBot b) (Gives (Bot c) _ _)
          | b == c = True
        bGives _ _ = False
        ac :: Int -> Reciever -> Bots -> Bots
        ac l x m =
          case lookup x m of
            Just y -> insertMap x (l : y) m
            Nothing -> insertMap x [l] m

loop :: Monad m => (a -> m a) -> a -> m a
loop f i =
  do b <- f i
     loop f b

result1 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy` endOfLine) <$> input)
     pure (loop (update i) $ foldl' initialize mempty i)


update' :: [Input] -> Bots -> Either Int Bots
update' i bs =
  let (b,vs) =
        case find (\(_,vs) -> length vs == 2)
                  (mapToList bs) of
          Just p -> p
          Nothing -> error "no bot with 2!"
      [l,h] = sort vs
      prod =
        do (a:_) <- lookup (Output 0) bs
           (b:_) <- lookup (Output 1) bs
           (c:_) <- lookup (Output 2) bs
           pure (a * b * c)
  in case prod of
       Just p -> Left p
       Nothing ->
         case find (bGives b) i of
           Just (Gives _ x y) -> pure (ac l x . ac h y $ insertMap b [] bs)
  where bGives (RBot b) (Gives (Bot c) _ _)
          | b == c = True
        bGives _ _ = False
        ac :: Int -> Reciever -> Bots -> Bots
        ac l x m =
          case lookup x m of
            Just y -> insertMap x (l : y) m
            Nothing -> insertMap x [l] m

result2 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy` endOfLine) <$> input)
     pure (loop (update' i) $ foldl' initialize mempty i)
