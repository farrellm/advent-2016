{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables, ViewPatterns#-}

module AdventPrelude2
  ( module Protolude
  , module Control.Monad.Trans.Either
  , module Data.Attoparsec.Text
  , module Lens.Micro.Platform
  , stepSearch
  ) where

import Protolude

import Control.Monad.Trans.Either hiding (right, left)
import Data.Attoparsec.Text
       (Parser, Result(..), parse, parseOnly, many1, sepBy, sepBy1, char,
        digit, letter, space, string, skipSpace, decimal, hexadecimal,
        double, signed, endOfLine, endOfInput)
import Lens.Micro.Platform hiding ((&), to)

import Data.Heap (MinPrioHeap)
import qualified Data.Heap as H
import qualified Data.Set as Set

stepSearch
  :: forall a b.
     Ord b
  => (a -> [a]) -> (a -> Int) -> (a -> Bool) -> (a -> b) -> a -> [a]
stepSearch s h g pi i = go (H.singleton (h i + 1, [i])) mempty
  where
    go :: MinPrioHeap Int [a] -> Set b -> [a]
    go (H.view -> Just ((_, p@(a:_)), q)) c
      | g a = p
      | otherwise =
        let ns = filter ((`Set.notMember` c) . pi) (s a)
            l = length p
            ps = fmap (\n -> (l + 1 + h n, n : p)) ns
        in go (foldl' (flip H.insert) q ps)
              (foldl' (flip Set.insert) c (map pi ns))
