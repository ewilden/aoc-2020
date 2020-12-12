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

module Day12 where

import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Semigroup (Sum (..))
import Data.Tuple
-- import Debug.Trace
import Import
import Lens.Micro.Platform
import qualified RIO.HashMap as HM
import qualified RIO.HashSet as HashSet
import RIO.List (headMaybe, iterate, sort, takeWhile)
import RIO.List.Partial (last, maximum)
import qualified RIO.Map as M
import RIO.Partial (read)
import RIO.State
import qualified RIO.Text as Text
import qualified RIO.Vector.Boxed as VB
import RIO.Writer

data Action = N | S | E | W | L | R | F deriving (Show, Eq, Read)

type NavInstr = (Action, Double)

input :: IO [NavInstr]
input = do
  rawInput <- readFileUtf8 "input/12.txt"
  return $
    flip fmap (Text.lines rawInput) $ \line ->
      case Text.unpack line of
        h : tl -> (read [h], read tl)
        _ -> error "bad line"

nav :: [NavInstr] -> Reader Double (Double, Double)
nav [] = pure (0, 0)
nav ((a, n) : tl) = case a of
  N -> second (+ n) <$> nav tl
  S -> second (- n +) <$> nav tl
  E -> first (+ n) <$> nav tl
  W -> first (- n +) <$> nav tl
  L -> local (+ n) $ nav tl
  R -> local (- n +) $ nav tl
  F -> do
    dirDegrees <- ask
    let dirRads = dirDegrees * pi / 180
        dx = n * cos dirRads
        dy = n * sin dirRads
    bimap (+ dx) (+ dy) <$> nav tl

answer1 :: [NavInstr] -> Double
answer1 inp =
  let (x, y) = runReader (nav inp) 0
   in abs x + abs y

nav2 :: [NavInstr] -> Reader (Double, Double) (Double, Double)
nav2 [] = pure (0, 0)
nav2 ((a, n) : tl) = case a of
  N -> local (second (+ n)) $ nav2 tl
  S -> local (second (- n +)) $ nav2 tl
  E -> local (first (+ n)) $ nav2 tl
  W -> local (first (- n +)) $ nav2 tl
  L -> local (rotate n) $ nav2 tl
  R -> local (rotate (- n)) $ nav2 tl
  F -> do
    (dx, dy) <- ask
    (x, y) <- nav2 tl
    return (x + n * dx, y + n * dy)
  where
    rotate deg (x, y) =
      let rad = deg * pi / 180
          x' = x * cos rad - y * sin rad
          y' = x * sin rad + y * cos rad
       in (x', y')

answer2 :: [NavInstr] -> Double
answer2 inp =
  let (x, y) = runReader (nav2 inp) (10, 1)
   in abs x + abs y