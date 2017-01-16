{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections, GADTs #-}
{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module Day25 where

import AdventPrelude2
import qualified Data.Map as M

input :: IO Text
input = readFile "data/day25.txt"

data Val = Reg Char | Con Int
  deriving (Show)

data Op = Cpy Val Char | Inc Char | Dec Char | Jnz Val Val | Tgl Val | Out Val
  deriving (Show)

parser =
  Cpy <$> (string "cpy " *> val) <*> (string " " *> letter) <|>
  Inc <$> (string "inc " *> letter) <|>
  Dec <$> (string "dec " *> letter) <|>
  Jnz <$> (string "jnz " *> val) <*> (string " " *> val) <|>
  Tgl <$> (string "tgl " *> val) <|>
  Out <$> (string "out " *> val)
  where val = Reg <$> letter <|> Con <$> signed decimal

type Registers = Map Char Int

data BunnyState = BunnyState
  { _instr :: Int
  , _reg :: Registers
  , _ops :: [Op]
  } deriving (Show)
makeLenses ''BunnyState

eval :: Op -> State BunnyState [Int]
eval op =
  case op of
    (Cpy v r) -> reg . ix r <~ val v >> pure []
    (Inc r) -> reg . ix r %= succ >> pure []
    (Dec r) -> reg . ix r %= pred >> pure []
    (Jnz vz vd) -> do
      z <- val vz
      d <- val vd
      if z /= 0
        then instr %= (+ (d - 1)) >> pure []
        else pure []
    (Tgl vt) -> do
      i <- use instr
      d <- val vt
      ops . ix (i + d - 1) %= tgl
      pure []
    (Out v) -> pure <$> val v

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

runOpt :: State BunnyState [Int]
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
        _ -> do
          op <- preuse (ops . ix i)
          case op of
            Nothing -> pure []
            Just o -> liftM2 (<>) (eval o) go
    add a b = do
      Just va <- preuse (reg . ix a)
      Just vb <- preuse (reg . ix b)
      let v = va + vb
      v `seq` reg . ix a .= v
      reg . ix b .= 0
      instr %= (+ 2)

signal i a =
  evalState
    runOpt
    BunnyState
    { _ops = i
    , _instr = 0
    , _reg = M.fromList [('a', a), ('b', 0), ('c', 0), ('d', 0)]
    }

result1 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy` endOfLine) <$> input)
     -- pure (take 5 $ map (uncurry (==)) (signal i 4))
     pure
       (take 1 $
        filter (\a -> and .
                      take 20 .
                      map (uncurry (==)) .
                      zip (cycle [0, 1]) $
                      signal i a) [0 ..])
