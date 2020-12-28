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

module Day22 where

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

input :: IO ([Int], [Int])
input = do
  rawInput <- readFileUtf8 "input/22.txt"
  let [rawP1, rawP2] = splitOn "\n\n" rawInput
      p1 = Text.lines rawP1 & drop 1 <&> (read . Text.unpack)
      p2 = Text.lines rawP2 & drop 1 <&> (read . Text.unpack)
  return (p1, p2)

runGame :: ([Int], [Int]) -> [Int]
runGame ([], l) = l
runGame (l, []) = l
runGame (h1 : t1, h2 : t2)
  | h1 > h2 = runGame (t1 ++ [h1, h2], t2)
  | otherwise = runGame (t1, t2 ++ [h2, h1])

answer1 :: IO Int
answer1 = do
  inp <- input
  let winnerDeck = runGame inp
  return $ sum $ zipWith (*) [1 ..] $ reverse winnerDeck

recCombat :: Reader (([Int], [Int]), HashSet ([Int], [Int])) (Either [Int] [Int])
recCombat = do
  currDecks@(p1, p2) <- view _1
  seenStates <- view _2
  if currDecks `HashSet.member` seenStates
    then return $ Left p1
    else local (over _2 (HashSet.insert currDecks)) $
      case (p1, p2) of
        (_, []) -> return $ Left p1
        ([], _) -> return $ Right p2
        (h1 : t1, h2 : t2) -> do
          p1Won <-
            if h1 <= length t1 && h2 <= length t2
              then isLeft <$> local (first (const (take h1 t1, take h2 t2))) recCombat
              else return $ h1 > h2
          local
            ( if p1Won
                then first (const (t1 ++ [h1, h2], t2))
                else first (const (t1, t2 ++ [h2, h1]))
            )
            recCombat

answer2 :: IO Int
answer2 = do
  inp <- input
  let winnerDeck = either id id $ runReader recCombat (inp, mempty)
  return $ sum $ zipWith (*) [1 ..] $ reverse winnerDeck