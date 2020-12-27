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

module Day21 where

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
import RIO.List (headMaybe, iterate, repeat, sort, sortOn, takeWhile)
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

newtype Ingredient = Ingredient
  { unIngredient :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable Ingredient

newtype Allergen = Allergen
  { unAllergen :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable Allergen

input :: IO [([Ingredient], [Allergen])]
input = do
  rawInput <- readFileUtf8 "input/21.txt"
  return $
    flip fmap (Text.lines rawInput) $ \line ->
      let [rawIngredients, rawAllergens'] = splitOn " (contains " line
          rawAllergens = Text.take (Text.length rawAllergens' - 1) rawAllergens'
          ingredients = fmap Ingredient $ Text.words rawIngredients
          allergens = fmap Allergen $ splitOn ", " rawAllergens
       in (ingredients, allergens)

newtype Intersection a = Intersection
  { unIntersection :: HashSet a
  }
  deriving (Show)

instance (Eq a, Hashable a) => Semigroup (Intersection a) where
  (Intersection x) <> (Intersection y) = Intersection $ HashSet.intersection x y

answer1 :: IO (MonoidalMap Allergen (Maybe (Intersection Ingredient)), Int)
answer1 = do
  inp <- input
  let buildIngSets (is, as) = MMap.fromList $ zip as (repeat $ Just $ Intersection $ HashSet.fromList is)
      ingSetMaps = fmap buildIngSets inp
      all2ing = mconcat ingSetMaps
      unsafeIngs = MMap.elems all2ing <&> fromMaybe undefined <&> unIntersection & HashSet.unions
      allIngs = HashSet.fromList $ join $ fst <$> inp
      safeIngs = allIngs `HashSet.difference` unsafeIngs
      safeIngsToOccurrence = flip foldMap inp $ \(is, _) ->
        is
          & filter (`HashSet.member` safeIngs)
          <&> (,Sum 1)
          & MMap.fromList
  return $ (all2ing, getSum $ mconcat $ MMap.elems safeIngsToOccurrence)

answer2 :: IO Text
answer2 = do
  (all2ing', _) <- answer1
  let all2ing = all2ing' <&> fromJust <&> unIntersection
      pickAllIngPairs :: State (HashMap Allergen (HashSet Ingredient), HashMap Allergen Ingredient) (HashMap Allergen Ingredient)
      pickAllIngPairs = do
        currMap <- use _1
        if HM.null currMap
          then use _2
          else do
            let (a, iset) = minimumBy (compare `on` (length . snd)) (HM.toList currMap)
                i = head $ HashSet.toList iset
            _2 . at a .= Just i
            _1
              %= HM.mapMaybe
                ( \is ->
                    let is' = HashSet.delete i is
                     in if HashSet.null is' then Nothing else Just is'
                )
            pickAllIngPairs
      allIngPairs = evalState pickAllIngPairs (HM.fromList $ MMap.toList $ all2ing, mempty)
  -- print $ HashSet.size <$> MMap.elems all2ing
  -- print $ all2ing & MMap.elems <&> HashSet.toList <&> (<&> unIngredient)
  -- print allIngPairs
  return $ HM.toList allIngPairs & sortOn fst <&> snd <&> unIngredient & Text.intercalate ","

-- return ""