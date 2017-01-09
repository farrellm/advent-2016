{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Day23 where

import AdventPrelude2
import qualified Data.Map as Map

input :: IO Text
input = readFile "data/day23.txt"

test :: IO Text
test = pure
  "cpy 2 a\n\
  \tgl a\n\
  \tgl a\n\
  \tgl a\n\
  \cpy 1 a\n\
  \dec a\n\
  \dec a"

test12 :: IO Text
test12 = pure
  "cpy 41 a\n\
  \inc a\n\
  \inc a\n\
  \dec a\n\
  \jnz a 2\n\
  \dec a"

data Val = Reg Char | Con Int
  deriving (Show)

data Op = Cpy Val Char | Inc Char | Dec Char | Jnz Val Val | Tgl Val
  deriving (Show)

parser =
  Cpy <$> (string "cpy " *> val) <*> (string " " *> letter) <|>
  Inc <$> (string "inc " *> letter) <|>
  Dec <$> (string "dec " *> letter) <|>
  Jnz <$> (string "jnz " *> val) <*> (string " " *> val) <|>
  Tgl <$> (string "tgl " *> val)
  where val = Reg <$> letter <|> Con <$> signed decimal

type Registers = Map Char Int

data BunnyState = BunnyState
  { _instr :: Int
  , _reg :: Registers
  , _ops :: [Op]
  } deriving (Show)
makeLenses ''BunnyState

eval :: Op -> State BunnyState ()
eval op =
  case op of
    (Cpy v r) -> do
      x <- val v
      reg . ix r .= x
    (Inc r) -> do
      -- fix space leak
      Just v <- preuse (reg . ix r)
      let v' = v + 1
      v' `seq` reg . ix r .= v'
    (Dec r) -> do
      -- fix space leak
      Just v <- preuse (reg . ix r)
      let v' = v - 1
      v' `seq` reg . ix r .= v'
    (Jnz vz vd) -> do
      z <- val vz
      d <- val vd
      if z /= 0
        then instr %= (+ (d - 1))
        else pure ()
    (Tgl vt) -> do
      i <- use instr
      d <- val vt
      ops . ix (i + d - 1) %= tgl

val :: Val -> State BunnyState Int
val v =
  case v of
    Con c -> pure c
    Reg r -> do
      Just w <- preuse (reg . ix r)
      pure w

tgl :: Op -> Op
tgl op =
  case op of
    (Inc v) -> Dec v
    (Dec v) -> Inc v
    (Tgl (Reg v)) -> Inc v
    (Jnz a (Reg b)) -> Cpy a b
    (Cpy a b) -> Jnz a (Reg b)

run :: State BunnyState ()
run = go
  where
    go = do
      i <- instr <<%= succ
      op <- preuse (ops . ix i)
      case op of
        Nothing -> pure ()
        Just o -> eval o >> go


result1 =
  runEitherT $
  do -- i <- EitherT (parseOnly (parser `sepBy` endOfLine) <$> test12)
     -- i <- EitherT (parseOnly (parser `sepBy` endOfLine) <$> test)
     i <- EitherT (parseOnly (parser `sepBy` endOfLine) <$> input)
     pure
       (execState
          run
          (BunnyState
           { _ops = i
           , _instr = 0
           , _reg = Map.fromList [('a', 7), ('b', 0), ('c', 0), ('d', 0)]
           }))

runOpt :: State BunnyState ()
runOpt = go
  where
    go = do
      i <- instr <<%= succ
      os <- drop i <$> use ops
      case os of
        Inc a:Dec b:Jnz (Reg b') (Con (-2)):_
          | b == b' -> add a b >> go
        Dec b:Inc a:Jnz (Reg b') (Con (-2)):_
          | b == b' -> add a b >> go
        -- Cpy (Con 0) a:Cpy (Reg b) c:
        --   Inc a':Dec c':Jnz (Reg c'') (Con (-2)):
        --   Dec d:Jnz (Reg d') (Con (-5)):_
        --   | a == a' && c == c' && c == c'' && d == d' ->
        --   mul a b c d
        _ -> do
          op <- preuse (ops . ix i)
          case op of
            Nothing -> pure ()
            Just o -> eval o >> go
    add a b = do
      Just va <- preuse (reg . ix a)
      Just vb <- preuse (reg . ix b)
      let v = va + vb
      v `seq` reg . ix a .= v
      reg . ix b .= 0
      instr %= (+ 2)
    -- mul a b c d = do


result2 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy` endOfLine) <$> input)
     pure
       (execState
          runOpt
          (BunnyState
           { _ops = i
           , _instr = 0
           , _reg = Map.fromList [('a', 12), ('b', 0), ('c', 0), ('d', 0)]
           }))
