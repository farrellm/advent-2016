{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day12 where

import ClassyPrelude

import Control.Monad.Trans.Either
import Data.Attoparsec.Text
       (Parser, parseOnly, sepBy, digit, letter, string, decimal, signed,
        endOfLine)

(!) :: IsSequence seq => seq -> Index seq -> Element seq
(!) = indexEx

input :: IO Text
input = readFile "data/day12.txt"

test :: IO Text
test = pure
  "cpy 41 a\n\
  \inc a\n\
  \inc a\n\
  \dec a\n\
  \jnz a 2\n\
  \dec a"

data Val = Reg Char | Con Int
  deriving (Show)

data Op = Cpy Val Char | Inc Char | Dec Char | Jnz Val Val
  deriving (Show)

parser =
  Cpy <$> (string "cpy " *> val) <*> (string " " *> letter) <|>
  Inc <$> (string "inc " *> letter) <|>
  Dec <$> (string "dec " *> letter) <|>
  Jnz <$> (string "jnz " *> val) <*> (string " " *> val)
  where val = Reg <$> letter <|> Con <$> signed decimal

type Registers = Map Char Int

registers :: Registers
registers = mapFromList [('a', 0), ('b', 0), ('c', 0), ('d', 0)]

update :: [Op] -> (Int, Registers) -> Registers
update ops (i, rs) =
  if i == length ops
    then rs
    else case ops ! i of
           Cpy v r -> update ops (i + 1, insertMap r (getVal rs v) rs)
           Inc r -> update ops (i + 1, adjustMap succ r rs)
           Dec r -> update ops (i + 1, adjustMap pred r rs)
           Jnz x y ->
             if getVal rs x /= 0
               then update ops (i + getVal rs y, rs)
               else update ops (i + 1, rs)
  where
    getVal :: Registers -> Val -> Int
    getVal _ (Con v) = v
    getVal rs (Reg r) =
      case lookup r rs of
        Just v -> v

result1 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy` endOfLine) <$> input)
     -- i <- EitherT (parseOnly (parser `sepBy` endOfLine) <$> test)
     pure (update i (0, registers))

registers' :: Registers
registers' = mapFromList [('a', 0), ('b', 0), ('c', 1), ('d', 0)]

result2 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy` endOfLine) <$> input)
     -- i <- EitherT (parseOnly (parser `sepBy` endOfLine) <$> test)
     pure (update i (0, registers'))
