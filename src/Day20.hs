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

flipPointLR :: Int -> (Int, Int) -> (Int, Int)
flipPointLR n (r, c) = (r, n - 1 - c)

flipPointUD :: Int -> (Int, Int) -> (Int, Int)
flipPointUD n (r, c) = (n - 1 - r, c)

vget :: VB.Vector (VU.Vector Bool) -> (Int, Int) -> Bool
vget grid (r, c) = grid VB'.! r VU'.! c

imap :: ((Int, Int) -> Bool -> Bool) -> VB.Vector (VU.Vector Bool) -> VB.Vector (VU.Vector Bool)
imap f grid = VB.imap (\r row -> VU.imap (\c cell -> f (r, c) cell) row) grid

rotateCW :: VB.Vector (VU.Vector Bool) -> VB.Vector (VU.Vector Bool)
rotateCW grid = imap (\point _ -> grid `vget` rotatePointCCW (VB.length grid) point) grid

rotateCCW :: Vector (VU.Vector Bool) -> Vector (VU.Vector Bool)
rotateCCW = rotateCW . rotateCW . rotateCW

flipLR :: VB.Vector (VU.Vector Bool) -> VB.Vector (VU.Vector Bool)
flipLR grid = imap (\point _ -> grid `vget` flipPointLR (VB.length grid) point) grid

flipUD :: VB.Vector (VU.Vector Bool) -> VB.Vector (VU.Vector Bool)
flipUD grid = imap (\point _ -> grid `vget` flipPointUD (VB.length grid) point) grid

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

data TileTransform = CWRotate | FlipLR deriving (Show, Eq, Generic)

tform :: TileTransform -> VB.Vector (VU.Vector Bool) -> VB.Vector (VU.Vector Bool)
tform CWRotate = rotateCW
tform FlipLR = flipLR

reverseTform :: TileTransform -> VB.Vector (VU.Vector Bool) -> VB.Vector (VU.Vector Bool)
reverseTform CWRotate = rotateCCW
reverseTform FlipLR = flipLR

tforms :: [TileTransform] -> VB.Vector (VU.Vector Bool) -> VB.Vector (VU.Vector Bool)
tforms ts grid = foldl' (flip tform) grid ts

reverseTforms :: [TileTransform] -> Vector (VU.Vector Bool) -> Vector (VU.Vector Bool)
reverseTforms ts grid = foldr reverseTform grid ts

answer2 :: IO Integer
answer2 = do
  inp <- input
  let id2tile = HM.fromList inp
      edgeToIxes = HM.fromList $
        MMap.toList $
          foldMap (uncurry MMap.singleton) $ do
            (tileID, tileGrid) <- inp
            (chosenEdge, numCWRotates) <- zip (getEdges tileGrid) [0 ..]
            [ (VU.toList chosenEdge, [(tileID, replicate numCWRotates CWRotate)]),
              (reverse $ VU.toList chosenEdge, [(tileID, replicate numCWRotates CWRotate ++ [FlipLR])])
              ]
      edgesWithNoMatches = HM.filter ((== 1) . length) edgeToIxes
      tileIDList = HM.foldl' (++) [] edgesWithNoMatches
      tileIDToCount = HM.fromList $ MMap.toList $ foldMap (\(tileID, _) -> MMap.singleton tileID (Sum 1)) tileIDList
      cornerTileIDs = HM.keys $ HM.filter (== Sum 4) tileIDToCount
      sideLength :: Int
      sideLength = round $ sqrt $ fromIntegral $ length inp
      suc :: (Int, Int) -> (Int, Int)
      suc (r, c)
        | c < sideLength - 1 = (r, c + 1)
        | otherwise = (r + 1, 0)
      startTileID = fromMaybe (error "head cornerTileIDs") $ headMaybe cornerTileIDs
      pretransStartTile = id2tile ^?! ix startTileID
      lsNumCwRotates =
        (fmap snd $ (join $ HM.elems edgesWithNoMatches) & filter ((== startTileID) . fst))
          & filter (all (/= FlipLR))
          & fmap length
          & fmap (`mod` 4)
      numCwRotatesForStart = maximum lsNumCwRotates
      transStartTile = tforms (replicate numCwRotatesForStart CWRotate) pretransStartTile
      finalTileGrid = evalState greedyTiles (HM.singleton (0, 0) (startTileID, transStartTile), HashSet.singleton startTileID, (0, 0))
      greedyTiles :: State (HashMap (Int, Int) (Int, VB.Vector (VU.Vector Bool)), HashSet Int, (Int, Int)) (HashMap (Int, Int) (Int, VB.Vector (VU.Vector Bool)))
      greedyTiles = do
        allocedTiles <- use _2
        prevPoint <- use _3
        if HashSet.size allocedTiles == length inp
          then use _1
          else do
            currMap <- use _1
            nextPoint <- _3 <%= suc
            if snd nextPoint > snd prevPoint
              then do
                let prevTile = currMap ^?! ix prevPoint
                    prevTileID = fst prevTile
                    prevEdge = (prevTile & snd & rotateCCW) ^?! ix 0 & VU.toList
                    matchingTileIDT = fromMaybe (error "matchingTileIDT1") $ headMaybe $ edgeToIxes ^?! ix (reverse prevEdge) & filter ((/= prevTileID) . fst)
                    nextTileID = fst matchingTileIDT
                    pretransTile = id2tile ^?! ix nextTileID
                    tformedTile = tforms (snd matchingTileIDT ++ replicate 3 CWRotate) pretransTile
                (_1 . at nextPoint) .= Just (nextTileID, tformedTile)
                _2 %= HashSet.insert nextTileID
                _3 .= nextPoint
                greedyTiles
              else do
                let realPrevPoint = first (-1 +) nextPoint
                    prevTile = currMap ^?! ix realPrevPoint
                    prevTileID = fst prevTile
                    prevEdge = (prevTile & snd & (rotateCW . rotateCW)) ^?! ix 0 & VU.toList
                    matchingTileIDT = fromMaybe (error "matchingTileIDT2") $ headMaybe $ edgeToIxes ^?! ix (reverse prevEdge) & filter ((/= prevTileID) . fst)
                    nextTileID = fst matchingTileIDT
                    pretransTile = id2tile ^?! ix nextTileID
                    tformedTile = tforms (snd matchingTileIDT) pretransTile
                (_1 . at nextPoint) .= Just (nextTileID, tformedTile)
                _2 %= HashSet.insert nextTileID
                _3 .= nextPoint
                greedyTiles
  print (length inp)
  print sideLength
  print (take 5 $ HM.keys edgesWithNoMatches)
  print (length cornerTileIDs)
  print cornerTileIDs
  print (HM.size finalTileGrid)
  return $ product (fmap fromIntegral cornerTileIDs)