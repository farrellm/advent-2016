{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections, GADTs #-}

module Day11 where

import ClassyPrelude

import Control.Monad.State (State, evalState, evalStateT, get, put, modify)
import Control.Monad.Trans.Either
import Data.Attoparsec.Text
       (Parser, parseOnly, many1, sepBy1, char, digit, letter, space,
        string, skipSpace, decimal, hexadecimal, double, signed, endOfLine)
import Data.Char (chr,ord)
import Data.List (elemIndex, transpose)
import qualified Data.List as L
import qualified Data.Set as S
-- import qualified Data.HashSet as S
import Data.Vector ((//))
import Lens.Micro.Platform

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs = let l = length xs
                          in if n>l then [] else subsequencesBySize xs `indexEx` (l-n)
 where
   subsequencesBySize [] = [[[]]]
   subsequencesBySize (x:xs) = let next = subsequencesBySize xs
                             in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])

data Obj = Gen Text | Chip Text
  deriving (Show, Eq, Ord)
data Floor = Floor Int [Obj]
  deriving (Show)


input :: IO Text
input = readFile "data/day11.txt"

test :: IO Text
test = pure
  "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.\n\
  \The second floor contains a hydrogen generator.\n\
  \The third floor contains a lithium generator.\n\
  \The fourth floor contains nothing relevant."

parser :: Parser Floor
parser =
  Floor <$> (string "The " *> floor <* string " floor contains ") <*>
  ((obj `sepBy1` sep) <|> (string "nothing relevant" *> pure [])) <*
  char '.'
  where
    floor =
      (string "first" *> pure 1) <|> (string "second" *> pure 2) <|>
      (string "third" *> pure 3) <|> (string "fourth" *> pure 4)
    obj =
      (Gen . pack) <$> (string "a " *> many1 letter <* " generator") <|>
      (Chip . pack) <$> (string "a " *> many1 letter <* "-compatible microchip")
    sep = many (char ',') *> many (char ' ') *> many (string "and ")

splitObjs :: Set Obj -> ([Text], [Text])
splitObjs os = foldl' splitObj ([],[]) os
  where splitObj (cs,gs) o = case o of
          Chip c -> (c:cs,gs)
          Gen g -> (cs,g:gs)

type FloorMap = Map Int (Set Obj)

type Status = (Int, FloorMap)

fromFloors :: [Floor] -> FloorMap
fromFloors fs = mapFromList (fmap toPair fs)
  where toPair (Floor i os) = (i, setFromList os)

isSafe :: Status -> Bool
isSafe (_, fs) = all floorSafe fs
  where floorSafe os =
          let (cs, gs) = splitObjs os
          in if null gs
             then True
             else all (`elem` gs) cs

isDone :: Int -> Status -> Bool
isDone n (_, fs) =
  let (Just os) = lookup 4 fs
  in length os == n

move :: Status -> [Status]
move (e, fs) =
  do e' <-
       case e of
         1 -> [2]
         4 -> [3]
         _ -> [e + 1, e - 1]
     let os =
           case lookup e fs of
             Just os -> setToList os
             Nothing -> error ("now floor: " <> show e <> " in " <> show fs)
     ms <- fmap pure os <> subsequencesOfSize 2 os
     let ms' = setFromList ms
         st' =
           (e', adjustMap (<> ms') e' $ adjustMap (`difference` ms') e fs)
     pure st'

loop :: Int -> (Int, [Status]) -> Set Status -> [Int]
loop n (i, ss) past =
  let i' = i + 1
      ss' = S.filter isSafe .
            (`S.difference` past) .
            setFromList $
            concatMap move ss
      (a, b) = partition (isDone n) (setToList ss')
  in traceShow (i, length ss) $
     (fmap (const i') a) <> loop n (i', b) (past <> ss')

result1 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy1` endOfLine) <$> input)
     -- i <- EitherT (parseOnly (parser `sepBy1` endOfLine) <$> test)
     let fs = fromFloors i
     -- pure (take 1 $ loop 10 (0, [(1, fs)]) mempty)
     pure (take 1 $ loop 10 (0, [(1, fs)]) mempty)
     -- pure fs

result2 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy1` endOfLine) <$> input)
     let fs = fromFloors i
         fs' =
           adjustMap
             (<> setFromList
                   [ Gen "elerium"
                   , Chip "elerium"
                   , Gen "dilithium"
                   , Chip "dilithium"
                   ])
             1
             fs
     pure (take 1 $ loop 14 (0, [(1, fs')]) mempty)
     -- pure fs'
