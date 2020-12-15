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

module Day15 where

-- import Debug.Trace

import Control.Monad (replicateM)
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
import RIO.List.Partial (last, maximum, minimumBy)
import qualified RIO.Map as M
import RIO.Partial (fromJust, read)
import RIO.State
import qualified RIO.Text as Text
import RIO.Text.Partial (splitOn)
import qualified RIO.Vector.Boxed as VB
import RIO.Writer

input :: [Int]
input = [0, 8, 15, 2, 12, 1, 4]

data GameState = GameState
  { _prevTurnNumber :: Int,
    _lastTurnSpoken :: HashMap Int Int,
    _lastNum :: Int
  }
  deriving (Show)

makeLenses ''GameState

turn :: State GameState ()
turn = do
  lastNo <- use lastNum
  currTurn <- prevTurnNumber <<%= (+ 1)
  lastSpoken <- lastTurnSpoken . at lastNo <<.= Just currTurn
  case lastSpoken of
    Nothing -> lastNum .= 0
    Just prevTurn -> lastNum .= currTurn - prevTurn

initialState :: GameState
initialState = GameState (length input) (HM.fromList $ zip (take (length input - 1) input) [1 ..]) (last input)

nthTurnResult :: Int -> Int
nthTurnResult turnNo =
  let (GameState _ _ n) = execState (replicateM (turnNo - length input) turn) initialState
   in n

answer1 :: Int
answer1 = nthTurnResult 2020

answer2 :: Int
answer2 = nthTurnResult 30000000