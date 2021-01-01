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

module Day23 where

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
import Import hiding (some, try)
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

newtype Label = Label
  { unLabel :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable Label

input :: [Label]
input = [2, 1, 5, 6, 9, 4, 7, 8, 3] <&> Label

pickDestLabel :: Label -> [Label] -> Label
pickDestLabel (Label i) pickedUpLabels =
  let go n
        | not (n `elem` (unLabel <$> pickedUpLabels)) = n
        | n == 1 = go 9
        | otherwise = go (n - 1)
   in Label $ if i == 1 then go 9 else go (i - 1)

runMove :: [Label] -> [Label]
runMove (currLabel : cups) =
  let pickedUpCups = take 3 cups
      cups' = filter (not . (`elem` pickedUpCups)) cups
      destLabel = pickDestLabel currLabel pickedUpCups
      cups'' = dropWhile (/= destLabel) cups' & drop 1
      cups''' = cycle $ pickedUpCups ++ take 6 cups''
   in cups''' & dropWhile (/= currLabel) & drop 1

answer1 :: Text
answer1 =
  iterate runMove (cycle input) !! 100
    & dropWhile (/= Label 1)
    & drop 1
    & take 8
    <&> unLabel
    <&> tshow
    & Text.intercalate ""

input2 :: [Label]
input2 = ([2, 1, 5, 6, 9, 4, 7, 8, 3] ++ [10 .. 1000000]) <&> Label

pickDestLabel2 :: Label -> [Label] -> Label
pickDestLabel2 (Label i) pickedUpLabels =
  let go n
        | not (n `elem` (unLabel <$> pickedUpLabels)) = n
        | n == 1 = go 1000000
        | otherwise = go (n - 1)
   in Label $ if i == 1 then go 1000000 else go (i - 1)

data BigState = BigState
  { _mappings :: Bimap.Bimap Rational Label,
    _currCup :: Label
  }

makeLenses ''BigState

cup2pos :: SimpleGetter BigState (Map.Map Label Rational)
cup2pos = mappings . to Bimap.toMapR

pos2cup :: SimpleGetter BigState (Map.Map Rational Label)
pos2cup = mappings . to Bimap.toMap

nextCup :: Label -> BigState -> Label
nextCup cup s =
  let pos = s ^?! cup2pos . ix cup
      cupRank = fromJust $ Map.lookupIndex pos (s ^. pos2cup)
   in snd $ Map.elemAt ((cupRank + 1) `mod` Map.size (s ^. pos2cup)) (s ^. pos2cup)

nextLowerLabelPresent :: Label -> BigState -> Label
nextLowerLabelPresent cup s =
  let labelRank = fromJust $ Map.lookupIndex cup (s ^. cup2pos)
   in fst $ Map.elemAt ((labelRank - 1) `mod` Map.size (s ^. cup2pos)) (s ^. cup2pos)

removeCup :: Label -> State BigState ()
removeCup cup = mappings %= Bimap.deleteR cup

nextCupsAfter :: Label -> BigState -> [Label]
nextCupsAfter cup s = drop 1 $ iterate (`nextCup` s) cup

pickUp3From :: Label -> State BigState [Label]
pickUp3From x = do
  pickedUpCups <- gets (take 3 . nextCupsAfter x)
  forM_ pickedUpCups removeCup
  return pickedUpCups

placeCupsAfter :: Label -> [Label] -> State BigState ()
placeCupsAfter x cups = do
  pos <- use cup2pos <&> \m -> m ^?! ix x
  pos' <- gets $ \s -> s ^?! cup2pos . ix (nextCup x s)
  if pos' < pos
    then -- wrap-around, so just give cups higher positions
    forM_ (zip cups [pos + 1 ..]) $ \(cup, rat) -> mappings %= Bimap.insert rat cup
    else
      let ind2pos i = fromInteger (i + 1) / fromIntegral (length cups + 2) * (pos' - pos) + pos
       in forM_ (zip [0 ..] cups) $ \(i, cup) -> mappings %= Bimap.insert (ind2pos i) cup

init :: [Label] -> BigState
init cups@(h : _) = BigState (Bimap.fromList $ zip [0 ..] cups) h

runMove' :: State BigState ()
runMove' = do
  curr <- use currCup
  pickedUpCups <- pickUp3From curr
  dest <- gets $ nextLowerLabelPresent curr
  placeCupsAfter dest pickedUpCups
  nextCup' <- gets (nextCup curr)
  currCup .= nextCup'

runMove'' :: BigState -> BigState
runMove'' = execState runMove'

runMove''' :: (Int, IntMap Int) -> (Int, IntMap Int)
runMove''' (curr, cups) =
  let (_ : a : b : c : afterPickedUp : _) = iterate (cups IntMap.!) curr
      pickedUpCups = [a, b, c]
      cups' = IntMap.insert curr afterPickedUp $ foldl' (flip IntMap.delete) cups pickedUpCups
      (dest, afterDest) = case IntMap.lookupLT curr cups' of
        Just pair -> pair
        Nothing -> IntMap.findMax cups'
      newPairs =
        [ (dest, a),
          (a, b),
          (b, c),
          (c, afterDest)
        ]
   in (afterPickedUp, foldl' (\m (l, r) -> IntMap.insert l r m) cups' newPairs)

bestAnswer :: Int -> [Int] -> [Int]
bestAnswer timesRun startingList =
  let startMap = IntMap.fromList (zip startingList $ drop 1 (cycle startingList))
      startCurr = head startingList
      (_, finalCups) = iterate runMove''' (startCurr, startMap) !! timesRun
   in take (length startingList) $ iterate (finalCups IntMap.!) 1

-- runMove2 :: [Label] -> [Label]
-- runMove2 (currLabel : cups) =
--   let pickedUpCups = take 3 cups
--       cups' = filter (not . (`elem` pickedUpCups)) cups
--       destLabel = pickDestLabel2 currLabel pickedUpCups
--       cups'' = dropWhile (/= destLabel) cups' & drop 1
--       cups''' = cycle $ pickedUpCups ++ take (1000000 - 3) cups''
--    in cups''' & dropWhile (/= currLabel) & drop 1

-- stepLabeled :: Int -> [Label] -> [Label]
-- stepLabeled maxLabelNum uncycledCups =
--   case cycle uncycledCups of
--     (currLabel : cups) ->
--       let pickedUpCups = take 3 cups
--           cups' = filter (not . (`elem` pickedUpCups)) cups
--           destLabel = pickDestLabelN maxLabelNum currLabel pickedUpCups
--           cups'' = dropWhile (/= destLabel) cups' & drop 1
--           cups''' = cycle $ pickedUpCups ++ take (maxLabelNum - 3) cups''
--        in cups''' & dropWhile (/= currLabel) & drop 1 & take maxLabelNum

-- step i ls = stepLabeled i (Label <$> ls) <&> unLabel

answer1' :: [Int]
answer1' =
  iterate runMove'' (init $ input) !! 100
    & (fmap snd . Bimap.toAscList . _mappings)
    & cycle
    <&> unLabel
    & dropWhile (/= 1)
    & take 9

answer2 :: [Int]
answer2 =
  iterate runMove'' (init $ input2) !! (10 * length input2)
    & (fmap snd . Bimap.toAscList . _mappings)
    & cycle
    <&> unLabel
    & dropWhile (/= 1)
    & drop 1
    & take 2

answer2' :: Integer
answer2' =
  let [1, a, b] = take 3 $ bestAnswer 10000000 $ unLabel <$> input2
   in fromIntegral a * fromIntegral b