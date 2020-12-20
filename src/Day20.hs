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

module Day20 where

-- import Debug.Trace

import Control.Monad (replicateM)
import Control.Monad.Combinators.Expr
import qualified Data.Bimap as Bimap
import Data.Bits
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Semigroup (Sum (..))
import Data.Tuple
import Import hiding (some, try)
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
import qualified RIO.Vector.Boxed.Partial as VB'
import qualified RIO.Vector.Unboxed as VU
import qualified RIO.Vector.Unboxed.Partial as VU'
import RIO.Writer
import Text.ParserCombinators.ReadP
import Prelude (print)

input :: IO [(Int, VB.Vector (VU.Vector Bool))]
input = do
  rawInput <- readFileUtf8 "input/20.txt"
  let rawNamedTiles' = splitOn "\n\n" rawInput
      rawNamedTiles = take (length rawNamedTiles' - 1) rawNamedTiles'
  return $
    flip
      fmap
      rawNamedTiles
      ( \rawNamedTile ->
          let (nameLine : rest) = Text.lines rawNamedTile
              name = read $ take 4 $ drop 5 $ Text.unpack nameLine
              vec = VB.fromList (fmap toVecRow rest)
              toVecRow rawLine = case Text.unpack rawLine of
                lineStr -> VU.fromList $ fmap (== '#') lineStr
           in (name, vec)
      )

rotatePointCCW :: Int -> (Int, Int) -> (Int, Int)
rotatePointCCW n (r, c) = (n - 1 - c, r)

rotatePointCW :: Int -> (Int, Int) -> (Int, Int)
rotatePointCW n = rotatePointCCW n . rotatePointCCW n . rotatePointCCW n

vget :: VB.Vector (VU.Vector Bool) -> (Int, Int) -> Bool
vget grid (r, c) = grid VB'.! r VU'.! c

imap :: ((Int, Int) -> Bool -> Bool) -> VB.Vector (VU.Vector Bool) -> VB.Vector (VU.Vector Bool)
imap f grid = VB.imap (\r row -> VU.imap (\c cell -> f (r, c) cell) row) grid

rotateCW :: VB.Vector (VU.Vector Bool) -> VB.Vector (VU.Vector Bool)
rotateCW grid = imap (\point _ -> grid `vget` rotatePointCCW (VB.length grid) point) grid

rotateCCW :: Vector (VU.Vector Bool) -> Vector (VU.Vector Bool)
rotateCCW = rotateCW . rotateCW . rotateCW

getEdges :: Vector (VU.Vector Bool) -> [VU.Vector Bool]
getEdges grid =
  [ grid VB'.! 0,
    rotateCW grid VB'.! 0,
    rotateCW (rotateCW grid) VB'.! 0,
    rotateCCW grid VB'.! 0
  ]

answer1 :: IO Integer
answer1 = do
  inp <- input
  let edgeToIxes = HM.fromList $
        MMap.toList $
          foldMap (uncurry MMap.singleton) $ do
            (tileID, tileGrid) <- inp
            chosenEdge <- getEdges tileGrid
            [ (VU.toList chosenEdge, [tileID]),
              (reverse $ VU.toList chosenEdge, [tileID])
              ]
      edgesWithNoMatches = HM.filter ((== 1) . length) edgeToIxes
      tileIDList = HM.foldl' (++) [] edgesWithNoMatches
      tileIDToCount = HM.fromList $ MMap.toList $ foldMap (\tileID -> MMap.singleton tileID (Sum 1)) tileIDList
      tileIDsOccurringTwice = HM.keys $ HM.filter (== Sum 4) tileIDToCount
  print (length inp)
  print (take 5 $ HM.keys edgesWithNoMatches)
  print (length tileIDsOccurringTwice)
  print tileIDsOccurringTwice
  return $ product (fmap fromIntegral tileIDsOccurringTwice)