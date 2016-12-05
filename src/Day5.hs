{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day5 where

import ClassyPrelude

import Control.Lens hiding (Index, index)
import Control.Monad.State (StateT, evalStateT, get, modify)
import qualified Crypto.Hash.MD5 as MD5
import Data.Attoparsec.Text hiding (take)
import Data.Char (ord)
import qualified Data.Vector as V
import Data.Word
import Numeric (showHex)

import Debug.Trace

(!) :: IsSequence seq => seq -> Index seq -> Element seq
(!) = indexEx

input :: String
input = "cxdnnyjw"
-- input = "abc"

xs :: String
xs = take 8 .
     catMaybes .
     map (`index` 5) .
     filter (isPrefixOf "00000") .
     map (concat . map (\i -> pad $ showHex i "")) .
     map unpack .
     map MD5.hash $
     map (\i-> fromString (input ++ show i) :: ByteString) [0..]
  where pad cs@[c] = '0' : cs
        pad cs = cs

ys :: [(Int, Char)]
ys = map (\w -> (ord (w `indexEx` 5) - ord '0', w `indexEx` 6)) .
     filter (isPrefixOf "00000") .
     map (concat . map (\i -> pad $ showHex i "")) .
     map unpack .
     map MD5.hash $
     map (\i-> fromString (input ++ show i) :: ByteString) [0..]
  where pad cs@[c] = '0' : cs
        pad cs = cs

y :: String
y = let e = replicate 8 ' ' :: Vector Char
        go 8 v _ = v
        go n v ((i, c):zs) =
          if i < 8 && v `indexEx` i == ' '
          then go (n+1) (v V.// [(i, c)]) zs
          else go n v zs
    in V.toList (go 0 e ys)
