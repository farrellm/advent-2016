{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day7 where

import ClassyPrelude

import Control.Monad.Trans.Either
import Data.Attoparsec.Text
       (Parser, parseOnly, many1, sepBy, char, letter, endOfLine)

input :: IO Text
input = readFile "data/day7.txt"
-- input = pure "abba[mnop]qrst\n\
--              \abcd[bddb]xyyx\n\
--              \aaaa[qwer]tyui\n\
--              \ioxxoj[asdfgh]zxcvbn"

data IP = IP [String] [String]
  deriving (Show)

sub =
  (Left <$> many1 letter) <|>
  (Right <$> (char '[' *> many1 letter <* char ']'))

parser = foldl' appendSub (IP [] []) <$> (many sub)
  where appendSub (IP as bs) e =
          case e of
            Left a -> IP (a : as) bs
            Right b -> IP as (b : bs)

result1 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy` endOfLine) <$> input)
     pure (length $ filter goodIP i)
  where hasABBA (a:rs@(b:c:d:_)) =
          if a == d && b == c && a /= b
             then True
             else hasABBA rs
        hasABBA _ = False
        goodIP (IP a b) = any hasABBA a && not (any hasABBA b)

result2 =
  runEitherT $
  do i <- EitherT (parseOnly (parser `sepBy` endOfLine) <$> input)
     pure (length $ filter goodIP i)
  where getABAs :: String -> [(Char,Char)]
        getABAs (a:rs@(b:c:_)) =
          if a == c && a /= b
             then (a,b) : getABAs rs
             else getABAs rs
        getABAs _ = []
        goodIP (IP l r) =
          (not . null) $
          do (a,b) <- concatMap getABAs l
             (c,d) <- concatMap getABAs r
             guard (a == d && b == c)
             pure ()
