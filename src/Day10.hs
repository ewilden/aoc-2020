{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day10 where

import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Semigroup (Sum (..))
import Import
import RIO.List (sort)
import RIO.List.Partial (last, maximum)
import RIO.Partial (read)
import qualified RIO.Text as Text

input :: IO [Int]
input = do
  rawInput <- readFileUtf8 "input/10.txt"
  return $ read . Text.unpack <$> Text.lines rawInput

sortedDiffDist :: [Int] -> MonoidalMap Int (Sum Int)
sortedDiffDist unsortedNums =
  let sortedNums = sort unsortedNums
      builtIn = 3 + last sortedNums
      nums = 0 : (sortedNums ++ [builtIn])
      prevNext = zip nums (drop 1 nums)
      diffs = fmap (uncurry $ flip (-)) prevNext
   in foldMap (`MMap.singleton` 1) diffs

pathsToEnd :: Int -> Int -> [Int] -> MonoidalMap Int (Sum Int)
pathsToEnd start end [] =
  if start >= end - 3
    then MMap.singleton end (Sum 1)
    else mempty
pathsToEnd start end (h : tl) =
  let fromH = pathsToEnd h end tl
      skippingH =
        MMap.toList fromH
          & filter (\(k, _) -> k - start <= 3)
          & MMap.fromList
      includingH =
        MMap.toList fromH
          & filter (const (h - start <= 3))
          & fmap (\(_, v) -> MMap.singleton h v)
          & mconcat
   in skippingH <> includingH

answer1 :: [Int] -> Int
answer1 inp =
  let dist = sortedDiffDist inp
   in getSum (MMap.findWithDefault 0 1 dist) * getSum (MMap.findWithDefault 0 3 dist)

answer2 :: [Int] -> Int
answer2 inp =
  pathsToEnd 0 (maximum inp + 3) (sort inp)
    & MMap.elems
    & mconcat
    & getSum