{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections, GADTs #-}

module Day16 where

import AdventPrelude

invert :: LText -> LText
invert = omap invC
  where invC '0' = '1'
        invC '1' = '0'

extend :: LText -> LText
extend a =
  let b = invert (reverse a)
  in concat [a, "0", b]

extendTo :: Int64 -> LText -> LText
extendTo n t
  | n <= fromIntegral (length t) = take n t
  | otherwise = extendTo n (extend t)

checksum :: String -> String
checksum t
  | length t `mod` 2 == 1 = t
  | otherwise = checksum (go t)
  where go (a:b:rs)
          | a == b = '1' : go rs
          | otherwise = '0' : go rs
        go _ = []

result1 =
  let -- i = "10000"
      -- l = 20
      i = "11110010111001001"
      l = 272
      d = extendTo l i
  in checksum (unpack d)

result2 =
  let i = "11110010111001001"
      l = 35651584
      d = extendTo l i
  in checksum (unpack d)
