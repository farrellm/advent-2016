{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections #-}

module Day10 where

import ClassyPrelude

import Control.Monad.Trans.Either
import Data.Attoparsec.Text
       (Parser, parseOnly, sepBy, string, decimal, endOfLine)

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

type Bots = Map Reciever [Int]

parser :: Parser Input
parser =
  Init <$> (Value <$> (string "value " *> decimal)) <*>
    (Bot <$> (string " goes to bot " *> decimal)) <|>
  Gives <$> (Bot <$> (string "bot " *> decimal)) <*>
    (string " gives low to " *> reciever) <*>
    (string " and high to " *> reciever)
  where reciever =
          RBot <$> (string "bot " *> decimal) <|>
          Output <$> (string "output " *> decimal)

initialize :: Bots -> Input -> Bots
initialize bs (Init (Value v) (Bot b)) =
  insertWith (<>) (RBot b) [v] bs
initialize bs _ = bs

update :: [Input] -> Bots -> Either Reciever Bots
update i bs =
  let (b,vs) =
        case find (\(_,vs) -> length vs == 2)
                  (mapToList bs) of
          Just p -> p
          Nothing -> error "no bot with 2!"
      svs@[vl,vh] = sort vs
  in if svs == [17,61]
        then Left b
        else case find (bGives b) i of
               Just (Gives _ bl bh) ->
                 pure (insertWith (<>) bl [vl] .
                       insertWith (<>) bh [vh] $
                       insertMap b [] bs)
               Nothing -> error ("no instruction for bot " <> show b)
  where bGives (RBot b) (Gives (Bot c) _ _)
          | b == c = True
        bGives _ _ = False

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
      [vl,vh] = sort vs
      prod =
        do (x:_) <- lookup (Output 0) bs
           (y:_) <- lookup (Output 1) bs
           (z:_) <- lookup (Output 2) bs
           pure (x * y * z)
  in case prod of
       Just p -> Left p
       Nothing ->
         case find (bGives b) i of
           Just (Gives _ bl bh) ->
             pure (insertWith (<>) bl [vl] .
                   insertWith (<>) bh [vh] $
                   insertMap b [] bs)
           Nothing -> error ("no instruction for bot " <> show b)
  where bGives (RBot b) (Gives (Bot c) _ _)
          | b == c = True
        bGives _ _ = False

result2 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy` endOfLine) <$> input)
     pure (loop (update' i) $ foldl' initialize mempty i)
