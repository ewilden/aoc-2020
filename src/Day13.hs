{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day13 where

import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Semigroup (Sum (..))
import Data.Tuple
-- import Debug.Trace
import Import
import Lens.Micro.Platform
import Math.NumberTheory.Moduli
import qualified RIO.HashMap as HM
import qualified RIO.HashSet as HashSet
import RIO.List (headMaybe, iterate, sort, takeWhile)
import RIO.List.Partial (last, maximum, minimumBy)
import qualified RIO.Map as M
import RIO.Partial (fromJust, read)
import RIO.State
import qualified RIO.Text as Text
import RIO.Text.Partial (splitOn)
import qualified RIO.Vector.Boxed as VB
import RIO.Writer

input :: IO (Int, [Maybe Int])
input = do
  rawInput <- readFileUtf8 "input/13.txt"
  let [t0, rawBusIds] = Text.lines rawInput
      busIds =
        rawBusIds & splitOn ","
          <&> Text.unpack
          <&> \case
            "x" -> Nothing
            n -> Just $ read n
  return (read $ Text.unpack t0, busIds)

answer1 :: (Int, [Maybe Int]) -> Int
answer1 (t0, busIds) =
  let withScores = fmap (fmap (\x -> (x - t0 `mod` x, x))) busIds
      cmp Nothing Nothing = EQ
      cmp Nothing _ = GT
      cmp _ Nothing = LT
      cmp (Just a) (Just b) = compare a b
      Just (t, i) = minimumBy cmp withScores
   in t * i

answer2 :: (Int, [Maybe Int]) -> Maybe Integer
answer2 (_, busIds) =
  busIds
    & zip [0 ..]
    & filter (isJust . snd)
    & fmap (second (fromIntegral . fromJust))
    & fmap (\(i, n) -> (- i, n))
    & chineseRemainder