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

module Day17 where

-- import Debug.Trace

import Control.Monad (replicateM)
import qualified Data.Bimap as Bimap
import Data.Bits
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Semigroup (Sum (..))
import Data.Tuple
import Import
import Lens.Micro.Platform
import Math.NumberTheory.Moduli
import qualified RIO.HashMap as HM
import qualified RIO.HashSet as HashSet
import RIO.List (headMaybe, iterate, sort, takeWhile)
import RIO.List.Partial (head, last, maximum, minimum, minimumBy, (!!))
import qualified RIO.Map as M
import RIO.Partial (fromJust, read)
import RIO.State
import qualified RIO.Text as Text
import RIO.Text.Partial (splitOn)
import qualified RIO.Vector.Boxed as VB
import RIO.Writer

type Rules = HashMap Text [(Int, Int)]

type UnvalTicket = [Int]

type Input = (Rules, UnvalTicket, [UnvalTicket])

input :: IO (HashMap (Int, Int, Int) ())
input = do
  rawInput <- readFileUtf8 "input/17.txt"
  let rows = fmap ((fmap (== '#')) . Text.unpack) $ Text.lines rawInput
  return $
    HM.fromList $ do
      (y, row) <- zip [0 ..] rows
      (x, cell) <- zip [0 ..] row
      if cell then [((x, y, 0), ())] else []

type CubeGrid = HashMap (Int, Int, Int) ()

step :: CubeGrid -> CubeGrid
step grid =
  let regionAround (x, y, z) = do
        x' <- [x - 1 .. x + 1]
        y' <- [y - 1 .. y + 1]
        z' <- [z - 1 .. z + 1]
        return (x', y', z')
      neighbors cell = HashSet.toList $ cell `HashSet.delete` HashSet.fromList (regionAround cell)
      cellsToConsider = HashSet.toList $
        HashSet.fromList $ do
          cell <- HM.keys grid
          neighbors cell
      numActiveNeighbors cell = length $ filter (\c -> grid ^. at c == Just ()) $ neighbors cell
      next cell
        | grid ^. at cell == Nothing = if numActiveNeighbors cell == 3 then Just () else Nothing
        | otherwise = if numActiveNeighbors cell `elem` [2, 3] then Just () else Nothing
   in cellsToConsider & fmap (\cell -> (cell, next cell)) & filter (isJust . snd) & fmap (second (const ())) & HM.fromList

type CubierGrid = HashMap (Int, Int, Int, Int) ()

stepCubier :: CubierGrid -> CubierGrid
stepCubier grid =
  let regionAround (x, y, z, w) = do
        x' <- [x - 1 .. x + 1]
        y' <- [y - 1 .. y + 1]
        z' <- [z - 1 .. z + 1]
        w' <- [w - 1 .. w + 1]
        return (x', y', z', w')
      neighbors cell = HashSet.toList $ cell `HashSet.delete` HashSet.fromList (regionAround cell)
      cellsToConsider = HashSet.toList $
        HashSet.fromList $ do
          cell <- HM.keys grid
          neighbors cell
      numActiveNeighbors cell = length $ filter (\c -> grid ^. at c == Just ()) $ neighbors cell
      next cell
        | grid ^. at cell == Nothing = if numActiveNeighbors cell == 3 then Just () else Nothing
        | otherwise = if numActiveNeighbors cell `elem` [2, 3] then Just () else Nothing
   in cellsToConsider & fmap (\cell -> (cell, next cell)) & filter (isJust . snd) & fmap (second (const ())) & HM.fromList

makeCubier :: CubeGrid -> CubierGrid
makeCubier cubeGrid = HM.toList cubeGrid & fmap (\((x, y, z), v) -> ((x, y, z, 0), v)) & HM.fromList

answer1 :: CubeGrid -> Int
answer1 inp = length $ HM.keys $ head $ drop 6 $ iterate step inp

answer2 :: CubeGrid -> Int
answer2 inp = length $ HM.keys $ head $ drop 6 $ iterate stepCubier (makeCubier inp)