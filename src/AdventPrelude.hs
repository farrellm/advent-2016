{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

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
import Data.Sequence (Seq, ViewL(..), viewl, (|>), (><))
import Data.Text.Format hiding (print)
import Data.Vector ((//))
import Data.Word
import Lens.Micro.Platform


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
