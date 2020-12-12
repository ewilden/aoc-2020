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

module Day11 where

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

data Spot = Floor | EmptySeat | OccupiedSeat deriving (Eq)

type Grid a = HashMap (Int, Int) a

-- type SpotGrid = VB.Vector (VB.Vector Spot)
type SpotGrid = Grid Spot

data ScoredSpotGrid = ScoredSpotGrid
  { _spotGrid :: Grid Spot,
    _scoreGrid :: Grid Int,
    _byScore :: Map Int (HashSet (Int, Int))
  }

makeLenses ''ScoredSpotGrid

initScored :: VicinityGetter -> SpotGrid -> ScoredSpotGrid
initScored vget sg = ScoredSpotGrid sg score mapByScore
  where
    score :: Grid Int
    score = HM.mapWithKey (\(r, c) _ -> numAdjOccupiedSeat vget r c sg) sg
    mapByScore :: Map Int (HashSet (Int, Int))
    mapByScore =
      HM.toList score
        <&> swap
        <&> second HashSet.singleton
        <&> uncurry MMap.singleton
        & mconcat
        & MMap.getMonoidalMap

type VicinityGetter = (Int, Int) -> SpotGrid -> [(Int, Int)]

vicinity1 :: VicinityGetter
vicinity1 (r, c) _ = do
  r' <- [r - 1 .. r + 1]
  c' <- [c - 1 .. c + 1]
  if r' == r && c' == c then [] else pure (r', c')

vicinity2 :: VicinityGetter
vicinity2 loc grid =
  let isInGrid :: (Int, Int) -> Bool
      isInGrid = isJust . (`HM.lookup` grid)
      firstChair :: [(Int, Int)] -> [(Int, Int)]
      firstChair = take 1 . filter ((`elem` [Just OccupiedSeat, Just EmptySeat]) . (`HM.lookup` grid)) . takeWhile isInGrid
      -- firstChair = take 1 . takeWhile isJust . filter ((`elem` [Just OccupiedSeat, Just EmptySeat]) . (`HM.lookup` grid))
      fromLoc :: ((Int, Int) -> (Int, Int)) -> [(Int, Int)]
      fromLoc f = firstChair $ drop 1 $ iterate f loc
      dirs :: [[(Int, Int)]]
      dirs =
        [ fromLoc $ first (-1 +),
          fromLoc $ first (1 +),
          fromLoc $ second (-1 +),
          fromLoc $ second (1 +),
          fromLoc $ bimap (-1 +) (-1 +),
          fromLoc $ bimap (1 +) (1 +),
          fromLoc $ bimap (-1 +) (1 +),
          fromLoc $ bimap (1 +) (-1 +)
        ]
   in join dirs

setSpot :: VicinityGetter -> (Int, Int) -> Spot -> State ScoredSpotGrid Bool
setSpot vget loc@(r, c) spot = do
  mayPrevSpot <- preuse $ spotGrid . ix loc
  diff <- case (mayPrevSpot, spot) of
    (Just EmptySeat, OccupiedSeat) -> do
      spotGrid . ix loc .= OccupiedSeat
      return 1
    (Just OccupiedSeat, EmptySeat) -> do
      spotGrid . ix loc .= EmptySeat
      return (-1)
    _ -> pure 0
  when (diff /= 0) $ do
    vicinity <- use $ spotGrid . to (vget loc)
    forM_ vicinity $ \loc' -> do
      scoreGrid . ix loc' %= (+ diff)
      mayNewScore <- preuse (scoreGrid . ix loc')
      case mayNewScore of
        Nothing -> pure ()
        Just newScore -> do
          let oldScore = newScore - diff
          byScore . ix oldScore %= HashSet.delete loc'
          byScore . at newScore %= Just . maybe (HashSet.singleton loc') (HashSet.insert loc')
  return (diff /= 0)

data IsDone = NotDone | Done deriving (Eq)

step :: Int -> VicinityGetter -> State ScoredSpotGrid IsDone
step minForMany vget = do
  noNeighbors <- maybe [] HashSet.toList <$> preuse (byScore . ix 0)
  changes <- forM noNeighbors (\x -> setSpot vget x OccupiedSeat)
  manyNeighbors <- join <$> forM [minForMany .. 8] (\n -> maybe [] HashSet.toList <$> preuse (byScore . ix n))
  changes' <- forM manyNeighbors (\x -> setSpot vget x EmptySeat)
  return $ if or (changes <> changes') then NotDone else Done

input :: IO SpotGrid
input = do
  rawInput <- readFileUtf8 "input/11.txt"
  let listForm = parseLine <$> Text.lines rawInput
      indexedListForm = zip [0 ..] (fmap (zip [0 ..]) listForm)
      flatIndexedList = fmap (\(r, row) -> fmap (\(c, spot) -> ((r, c), spot)) row) indexedListForm
  return $ HM.fromList $ join flatIndexedList
  where
    parseLine line =
      flip fmap (Text.unpack line) $ \case
        '.' -> Floor
        'L' -> EmptySeat
        '#' -> OccupiedSeat

numAdjOccupiedSeat :: VicinityGetter -> Int -> Int -> SpotGrid -> Int
numAdjOccupiedSeat vget r c vv =
  let selectedCells = join [vv ^.. ix loc | loc <- vget (r, c) vv]
   in sum . map (fromEnum . (== OccupiedSeat)) $ selectedCells

runUntilStable :: Int -> VicinityGetter -> ScoredSpotGrid -> ScoredSpotGrid
runUntilStable minForMany vget sg =
  -- trace "run step" $
  case runState (step minForMany vget) sg of
    (Done, s') -> s'
    (NotDone, s') -> runUntilStable minForMany vget s'

answer1 :: SpotGrid -> Int
answer1 inp =
  inp
    & initScored vicinity1
    & runUntilStable 4 vicinity1
    & _spotGrid
    & HM.toList
    & fmap snd
    & filter (== OccupiedSeat)
    & length

answer2 :: SpotGrid -> Int
answer2 inp =
  inp
    & initScored vicinity2
    & runUntilStable 5 vicinity2
    & _spotGrid
    & HM.toList
    & fmap snd
    & filter (== OccupiedSeat)
    & length

-- VB.toList (runUntilStable inp) >>= VB.toList & filter (== OccupiedSeat) & length