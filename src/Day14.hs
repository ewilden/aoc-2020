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

module Day14 where

import Data.Bits
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

data Mask = Mask
  { _zeroes :: [Int],
    _ones :: [Int]
  }
  deriving (Show)

data PgmLine = SetMask Mask | SetMem Integer Integer deriving (Show)

input :: IO [PgmLine]
input = do
  rawInput <- readFileUtf8 "input/14.txt"
  return $
    rawInput & Text.lines <&> splitOn " = "
      <&> ( \[l, r] -> case Text.take 3 l of
              "mas" -> SetMask (parseMask r)
              "mem" -> SetMem (l & Text.take (Text.length l - 1) & Text.drop 4 & Text.unpack & read) (read $ Text.unpack r)
          )
  where
    parseMask :: Text -> Mask
    parseMask rawMask =
      rawMask & Text.unpack & reverse & zip [0 ..]
        & filter ((/= 'X') . snd) <&> (\(ind, val) -> if val == '1' then Right ind else Left ind)
        & partitionEithers
        & uncurry Mask

maskVal :: Integer -> Mask -> Integer
maskVal val (Mask zs os) = foldl' setBit (foldl' clearBit val zs) os

runPgmLine :: PgmLine -> State (Mask, HashMap Integer Integer) ()
runPgmLine (SetMask mask) = _1 .= mask
runPgmLine (SetMem loc val) = do
  mask <- use _1
  _2 . at loc .= Just (maskVal val mask)

initialState :: (Mask, HashMap Integer Integer)
initialState = (Mask [] [], mempty)

answer1 :: [PgmLine] -> Integer
answer1 pgm = HM.foldl' (+) 0 (snd $ execState (mapM runPgmLine pgm) initialState)

runPgmLine2 :: PgmLine -> State (Mask, HashMap Integer Integer) ()
runPgmLine2 (SetMask mask) = _1 .= mask
runPgmLine2 (SetMem loc val) = do
  Mask zs os <- use _1
  let maskSet = HashSet.fromList (zs <> os)
      xBits = HashSet.toList $ HashSet.fromList [0 .. 35] `HashSet.difference` maskSet
      baseAddr = foldl' setBit loc os
      addrs = foldM fuzzBit baseAddr xBits
      fuzzBit :: Integer -> Int -> [Integer]
      fuzzBit addr xBit = do
        f <- [setBit, clearBit]
        return $ f addr xBit
  addrs `forM_` \addr -> _2 . at addr .= Just val

answer2 :: [PgmLine] -> Integer
answer2 pgm = HM.foldl' (+) 0 (snd $ execState (mapM runPgmLine2 pgm) initialState)