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

module Day16 where

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

input :: IO (Rules, UnvalTicket, [UnvalTicket])
input = do
  rawInput <- readFileUtf8 "input/16.txt"
  let [rawRules, rawYourTicket, rawNearbyTickets] = splitOn "\n\n" rawInput
      parseRule :: Text -> (Text, [(Int, Int)])
      parseRule rawRule =
        let [k, rawV] = splitOn ": " rawRule
            rawRanges = splitOn " or " rawV
            parseRange rawRange =
              let [rawL, rawR] = splitOn "-" rawRange
               in bimap (read . Text.unpack) (read . Text.unpack) (rawL, rawR)
         in (k, fmap parseRange rawRanges)
      rules = HM.fromList $ fmap parseRule (Text.lines rawRules)
      parseTicket :: Text -> UnvalTicket
      parseTicket = fmap (read . Text.unpack) . splitOn ","
  return (rules, parseTicket (Text.lines rawYourTicket !! 1), fmap parseTicket (drop 1 $ Text.lines rawNearbyTickets))

isWithin :: Int -> (Int, Int) -> Bool
isWithin x (l, r) = l <= x && x <= r

impossibleValues :: UnvalTicket -> Reader Rules [Int]
impossibleValues fields = do
  rules <- ask
  let ranges = join $ HM.elems rules
  return $ filter (\field -> not (any (field `isWithin`) ranges)) fields

answer1 :: Input -> Int
answer1 (rules, _, nearbyTickets) = sum $ runReader (foldMapM impossibleValues nearbyTickets) rules

newtype TicketOrder = TicketOrder
  { _fields :: [HashSet Text]
  }
  deriving (Show)

traceTshowId :: (Show a) => a -> a
-- traceTshowId a = trace (tshow a) a
traceTshowId = id

finalize :: TicketOrder -> [Text]
finalize ticketOrder =
  -- trace (tshow $ fmap length fieldSets) $ (head . HashSet.toList) <$> fieldSets
  let go :: TicketOrder -> ReaderT (Bimap.Bimap Int Text) [] [Text]
      go (TicketOrder fieldSets) = do
        let indices :: [Int] = zipWith const [0 ..] fieldSets
        mapping <- ask
        if Bimap.size mapping == length fieldSets
          then return $ fmap (fromJust . (`Bimap.lookup` mapping)) indices
          else do
            let ixesAndOptions = traceTshowId $ filter (\(i, _) -> isNothing (i `Bimap.lookup` mapping)) $ zip indices fieldSets
                minNumOptions = traceTshowId $ minimum (fmap (HashSet.size . snd) ixesAndOptions)
                minimalOptions =
                  traceTshowId $ filter (\(_, opts) -> HashSet.size opts == minNumOptions) ixesAndOptions
            (fixIx, fieldOpts) <- lift minimalOptions
            fixField <- lift $ HashSet.toList fieldOpts
            let newMapping = traceTshowId $ Bimap.insert fixIx (traceTshowId fixField) mapping
                newTicketOrder = traceTshowId $ TicketOrder $ fmap (\s -> s `HashSet.difference` (HashSet.fromList $ Bimap.keysR newMapping)) fieldSets
            local (const $ traceTshowId newMapping) $ go (traceTshowId newTicketOrder)
   in head $ flip runReaderT Bimap.empty $ go ticketOrder

-- go (TicketOrder fieldSets)
--   | all ((== 1) . HashSet.size) fieldSets = pure $ TicketOrder fieldSets
--   | otherwise = do
--       setsAlreadyUsed <- ask
--       let minNumChoices = minimum (fmap (`HashSet.difference` setsAlreadyUsed) fieldSets
--                               & filter ((> 0) .)

--         let pickedField = head $ HashSet.toList $ head $ filter ((== 1) . HashSet.size) fieldSets
--             pick fieldSet = if HashSet.size fieldSet == 1 then fieldSet else pickedField `HashSet.delete` fieldSet
--          in go $ traceTshowId $ TicketOrder $ fmap pick fieldSets
--  in (head . HashSet.toList) <$> _fields (go ticketOrder)

instance Semigroup TicketOrder where
  (TicketOrder a) <> (TicketOrder b) = TicketOrder $ zipWith HashSet.intersection a b

newtype MayTicketOrder = MayTicketOrder
  { unMTO :: (Maybe TicketOrder)
  }
  deriving (Show)

instance Semigroup MayTicketOrder where
  (MayTicketOrder (Just a)) <> (MayTicketOrder (Just b)) = MayTicketOrder (Just (a <> b))
  (MayTicketOrder Nothing) <> t = t
  t <> (MayTicketOrder Nothing) = t

instance Monoid MayTicketOrder where
  mempty = MayTicketOrder Nothing

possibleTicketOrder :: UnvalTicket -> Reader Rules TicketOrder
possibleTicketOrder vals = TicketOrder <$> mapM getPossibleFieldNames vals
  where
    getPossibleFieldNames :: Int -> Reader Rules (HashSet Text)
    getPossibleFieldNames val = do
      rules <- ask
      return $ rules & HM.toList & filter (\(_, v) -> any (val `isWithin`) v) & fmap fst & HashSet.fromList

deriveTicketOrder :: Input -> [Text]
deriveTicketOrder (rules, _, nearbyTickets) = flip runReader rules $ do
  validTix <- filterM (fmap null . impossibleValues) nearbyTickets
  mayTicketOrder <- foldMapM (fmap (MayTicketOrder . Just) . possibleTicketOrder) validTix
  maybe (error "no valid ticket order") (return . finalize) (unMTO mayTicketOrder)

answer2 :: Input -> Int
answer2 inp@(rules, yourTicket, nearbyTickets) =
  let fieldList = deriveTicketOrder inp
      fieldValPairs = zip fieldList yourTicket
   in trace (tshow fieldList) $ product $ map snd $ filter ((== "departure") . (Text.take (length @[] "departure")) . fst) fieldValPairs