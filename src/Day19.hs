{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections, GADTs #-}

module Day19 where

import AdventPrelude
import qualified Data.Sequence as S

input :: Int
-- input = 5
input = 3012210


loop [x] [] = x
loop (a:_:rs) t = loop rs (a : t)
loop h t = loop (h <> reverse t) []

result1 =
  loop [1..input] []

loop' :: Seq Int -> Seq Int -> Int
loop' h t =
  case (viewl h, viewl t) of
    (x :< h', EmptyL)
      | null h' -> x
    (a :< rs, _) ->
      let i = (S.length h + S.length t) `div` 2
      in if S.length rs > (i - 1)
            -- traceShow (S.length h, S.length t, i) $
           then loop' (S.deleteAt (i - 1) rs) (t |> a)
           else loop' (h >< t) mempty
    _ -> loop' (h >< t) mempty

result2 =
  loop' (fromList [1..input]) mempty
