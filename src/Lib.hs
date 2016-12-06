{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}
module Lib
    ( someFunc
    ) where

import ClassyPrelude

import qualified Day5 as Day5

class ToString a where
  toString :: a -> Text

instance {-# OVERLAPPABLE #-} Show a => ToString a where
  toString = tshow

instance ToString Text where
  toString = id

instance ToString [Char] where
  toString = pack

day5 = Day5.y

someFunc :: IO ()
someFunc = putStrLn (toString  day5)
