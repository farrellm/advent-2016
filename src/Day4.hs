{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day4 where

import ClassyPrelude

import Control.Lens
import Control.Monad.State (StateT, evalStateT, get, modify)
import Data.Attoparsec.Text hiding (take)
import Data.Char

import Debug.Trace

input :: IO Text
input = readFile "data/day4.txt"

test = "aaaaa-bbb-z-y-x-123[abxyz]\n\
       \a-b-c-d-e-f-g-h-987[abcde]\n\
       \not-a-real-room-404[oarel]\n\
       \totally-real-room-200[decoy]"

name :: Parser String
name = intercalate "-" <$> (many1 letter `sepBy` char '-')

nameNoDash :: Parser String
nameNoDash = concat <$> (many1 letter `sepBy` char '-')

sector :: Parser Int
sector = decimal

checksum :: Parser String
checksum = char '[' *> many letter <* char ']'

data Entry =
  Entry {_name :: String
        ,_room :: Int
        ,_check :: String}
  deriving ((Show))

entry :: Parser Entry
entry = Entry <$> (name <* char '-') <*> sector <*> checksum

t1 = "aaaaa-bbb-z-y-x-123[abxyz]"
t4 = "totally-real-room-200[decoy]"
p1 = parseOnly entry t1
p4 = parseOnly entry t4


isValid :: Entry -> Bool
isValid e =
  let gs = group $ sort (filter (\c -> c /= '-') (_name e))
      cs = toCounts =<< gs
      cs' = take 5 $ sortBy (comparing (\(i,c) -> (-i, c))) cs
      check = map (^._2) cs'
  in _check e == check
  where toCounts xs@(x:_) = [(length xs, x)]
        toCounts _ = []

result1 =
  do -- ees <- parseOnly (entry `sepBy1` endOfLine) <$> input
     ees <- parseOnly (entry `sepBy1` endOfLine) <$> (pure test :: IO Text)
     pure $ (sum . map _room . filter isValid) <$> ees

shiftLetter :: Int -> Char -> Char
shiftLetter _ '-' = '-'
shiftLetter i c =
  let a = ord 'a' :: Int
      c' = ord c :: Int
  in chr (((c' - a) + i) `mod` 26 + a)

result2 =
  do ees <- parseOnly (entry `sepBy1` endOfLine) <$> input
     -- ees <- parseOnly (entry `sepBy1` endOfLine) <$> (pure test :: IO Text)
     -- pure $ (map decode . filter isValid) <$> ees
     pure $ (filter (\e -> decode e == "northpole-object-storage") . filter isValid) <$> ees
 where decode e = map (shiftLetter (_room e)) (_name e)
