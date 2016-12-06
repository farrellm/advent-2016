{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day5 where

import ClassyPrelude

import qualified Crypto.Hash.MD5 as MD5
import Data.Char (ord)
import qualified Data.Vector as V
import Data.Text.Format

import Debug.Trace

(!) :: IsSequence seq => seq -> Index seq -> Element seq
(!) = indexEx

input :: String
input = "cxdnnyjw"
-- input = "abc"

showHex :: Word8 -> LText
showHex i = format "{}" $ Only (left 2 '0' $ hex i)

xs :: String
xs = take 8 .
     map (! 5) .
     filter (isPrefixOf "00000") .
     map (concatMap showHex) .
     map MD5.hash $
     map (fromString . (input <>) . show) [0..]

-- Alternate structuring of xs
xs' =
  let inputs = (fromString . (input <>) . show) <$> [0 ..]
      hashes = MD5.hash <$> inputs
      hexes = concatMap showHex <$> hashes
      hexes' = filter (isPrefixOf "00000") hexes
      key = (! 5) <$> hexes'
  in take 5 key

ys :: [(Int, Char)]
ys = map (\w -> (ord (w ! 5) - ord '0', w ! 6)) .
     filter (isPrefixOf "00000") $
     map (concatMap showHex .
          MD5.hash .
          fromString .
          (input <>) .
          show) [0..]

y :: String
y =
  let e = replicate 8 ' ' :: Vector Char
      go 8 v _ = v
      go n v ((i,c):zs) =
        if i < 8 && v ! i == ' '
           then traceShow v $
                go (n + 1)
                   (v V.// [(i,c)])
                   zs
           else go n v zs
  in V.toList (go 0 e ys)
