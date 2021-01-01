{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day24 where

-- import Debug.Trace

import Control.Monad (replicateM)
import Control.Monad.Combinators.Expr
import qualified Data.Bimap as Bimap
import Data.Bits
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Semigroup (Sum (..))
import Data.Tuple
import Import hiding (many, some, try)
import Lens.Micro.Platform
import Math.NumberTheory.Moduli
import qualified RIO.HashMap as HM
import qualified RIO.HashSet as HashSet
import RIO.List (cycle, headMaybe, iterate, repeat, sort, sortOn, takeWhile)
import RIO.List.Partial (head, last, maximum, maximumBy, minimum, minimumBy, (!!))
import qualified RIO.Map as M
import qualified RIO.Map as Map
import RIO.Partial (fromJust, read)
import RIO.State
import qualified RIO.Text as Text
import RIO.Text.Partial (splitOn)
import qualified RIO.Vector.Boxed as VB
import qualified RIO.Vector.Boxed.Partial as VB'
import qualified RIO.Vector.Unboxed as VU
import qualified RIO.Vector.Unboxed.Partial as VU'
import RIO.Writer
import Text.ParserCombinators.ReadP
import Prelude (print)

data Dir = E | SE | SW | W | NW | NE deriving (Show, Eq)

allDirs :: [Dir]
allDirs = [E, SE, SW, W, NW, NE]

pDir :: ReadP Dir
pDir =
  choice
    [ string "e" $> E,
      string "se" $> SE,
      string "sw" $> SW,
      string "w" $> W,
      string "nw" $> NW,
      string "ne" $> NE
    ]

parseLine :: Text -> [Dir]
parseLine t = Text.unpack t & readP_to_S (many1 pDir <* eof) & head & fst

input :: IO [[Dir]]
input = do
  rawInput <- readFileUtf8 "input/24.txt"
  return $ parseLine <$> Text.lines rawInput

toDelta :: Dir -> (Int, Int)
toDelta E = (2, 0)
toDelta SE = (1, -1)
toDelta SW = (-1, -1)
toDelta W = (-2, 0)
toDelta NW = (-1, 1)
toDelta NE = (1, 1)

sumX :: [(Int, Int)] -> Int
sumX l = foldl' (+) 0 $ map fst l

sumY :: [(Int, Int)] -> Int
sumY l = foldl' (+) 0 $ map snd l

getTileMap :: IO (Map (Int, Int) Bool)
getTileMap = do
  inp <- input
  let tileToTimesFlipped :: MonoidalMap (Int, Int) (Sum Int)
      tileToTimesFlipped = foldMap toSingleton inp
      toSingleton tileWalk =
        let deltas = toDelta <$> tileWalk
            x = sumX deltas
            y = sumY deltas
         in MMap.singleton (x, y) (Sum 1)
  return $ MMap.getMonoidalMap tileToTimesFlipped <&> (odd . getSum)

answer1 :: IO Int
answer1 = do
  tileMap <- getTileMap
  return $ sum $ fromEnum <$> Map.elems tileMap

neighborhood :: (Int, Int) -> [(Int, Int)]
neighborhood pt = do
  dir <- allDirs
  let ptAndDelta = [pt, toDelta dir]
  return (sumX ptAndDelta, sumY ptAndDelta)

saturate :: Map (Int, Int) Bool -> Map (Int, Int) Bool
saturate m =
  let pts = Map.keys m
      blankMap = Map.fromList $ do
        pt <- pts
        nbr <- neighborhood pt
        return (nbr, False)
   in -- union is left-biased
      Map.union m blankMap

-- True is Black
day :: Map (Int, Int) Bool -> Map (Int, Int) Bool
day s = flip Map.mapWithKey s $ \pt isBlack ->
  let neighborVals = fromMaybe False . flip Map.lookup s <$> neighborhood pt
      n = sum $ fromEnum <$> neighborVals
   in if isBlack
        then not (n == 0 || n > 2)
        else n == 2

answer2 :: IO Int
answer2 = do
  tileMap <- getTileMap
  return $ sum $ fromEnum <$> Map.elems (iterate (saturate . day) (saturate tileMap) !! 100)
