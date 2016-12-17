{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections, GADTs #-}

module Day14 where

import AdventPrelude
import qualified Crypto.Hash.MD5 as MD5
import Data.Text.Format hiding (print)

salt :: String
-- salt = "abc"
salt = "ahsbgdzn"

hashes :: [Text]
hashes =
  (repack .
   concat .
   map (\b -> format "{}" (Only $ left 2 '0' (hex b))) .
   unpack .
   MD5.hash .
   fromString . (salt <>) . show) <$> [0..]

isGood :: Text -> [Text] -> Bool
isGood h hs = (not . null) $ do
  a <- threeX (unpack h)
  let as = replicate 5 a
      ts = take 1000 hs
  guard (any (isInfixOf as) ts)
  pure a
  where threeX (a: rs@(b:c:rs'))
          | a == b && a == c = [a]
          | otherwise = threeX rs
        threeX _ = []

filterGood :: [(Int, Text)] -> [(Int, Text)]
filterGood (h:hs)
  | isGood (snd h) (snd <$> hs) = h : filterGood hs
  | otherwise = filterGood hs
filterGood _ = []

result1 = (filterGood (zip [0..] hashes)) !! (64 - 1)

hashes' :: [Text]
hashes' =
  (decodeUtf8 .
   stretch 2017 .
   fromString . (salt <>) . show) <$> [0..]
  where stretch :: Int -> ByteString -> ByteString
        stretch 0 h = h
        stretch i h = let h' =
                            encodeUtf8 .
                            toStrict .
                            concat .
                            map (format "{}" . (Only . left 2 '0' . hex )) .
                            unpack $
                            MD5.hash h
                      in h' `seq` stretch (i-1) h'

result2 = take 64 (zip [1..] (filterGood (zip [0..] hashes')))
