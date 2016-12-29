{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, ViewPatterns#-}

module AdventPrelude
  ( module ClassyPrelude
  , module Control.Monad.Trans.Either
  , module Control.Monad.State
  , module Data.Attoparsec.Text
  , module Data.Bits
  , module Data.Char
  , module Data.List
  , module Data.Sequence
  , module Data.Vector
  , module Data.Word
  , module Lens.Micro.Platform
  , (!!)
  , (!)
  , showBytes
  , md5Hash
  , stepSearch
  ) where

import ClassyPrelude

import Control.Monad.Trans.Either hiding (right, left)
import Control.Monad.State
       (State, StateT, runState, evalState, execState, runStateT,
        evalStateT, execStateT, get, put, modify)
import Data.Attoparsec.Text
       (Parser, Result(..), parse, parseOnly, many1, sepBy, sepBy1, char,
        digit, letter, space, string, skipSpace, decimal, hexadecimal,
        double, signed, endOfLine, endOfInput)
import Data.Bits
import Data.Char (chr,ord)
import Data.List (elemIndex, transpose)
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as H
import Data.Sequence (Seq, ViewL(..), viewl, (|>), (><))
import Data.Text.Format hiding (print)
import Data.Vector ((//))
import Data.Word
import Lens.Micro.Platform
import qualified Crypto.Hash.MD5 as MD5


(!!) :: IsSequence seq => seq -> Index seq -> Element seq
(!!) = indexEx

(!) :: IsMap map => map -> ContainerKey map -> MapValue map
m ! k = case lookup k m of
          Just v -> v
          Nothing -> error "missing key in map"

showBytes :: ByteString -> LText
showBytes bs =
  concat .
  fmap (format "{}" . (Only . left 2 '0' . hex )) $
  unpack bs

md5Hash :: ByteString -> ByteString
md5Hash = MD5.hash

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
        let ns = filter ((`notMember` c) . pi) (s a)
            l = length p
            ps = fmap (\n -> (l + 1 + h n, n : p)) ns
        in go (foldl' (flip H.insert) q ps)
              (foldl' (flip insertSet) c (map pi ns))
