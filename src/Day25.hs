{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day25 where

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
import Import hiding (many, some, try)
import Lens.Micro.Platform
import Math.NumberTheory.Moduli
import Math.NumberTheory.Moduli.Class
import Math.NumberTheory.Moduli.DiscreteLogarithm
import Math.NumberTheory.Moduli.PrimitiveRoot
import Math.NumberTheory.Moduli.Singleton
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

transform :: Natural -> Natural -> Natural
transform loopSize subject = go loopSize 1
  where
    go 0 x = x
    go n x = go (n - 1) $ (x * subject) `rem` 20201227

loopSizes :: (Natural, Natural)
loopSizes =
  -- (cardPubKey, doorPubKey) <- input
  -- isPrimit
  let cg = fromJust cyclicGroup :: CyclicGroup Integer 20201227
      primRoot = fromJust (isPrimitiveRoot cg 7)
      cardPubKey = fromJust (isMultElement (8421034 :: Mod 20201227))
      doorPubKey = fromJust (isMultElement (15993936 :: Mod 20201227))
   in (discreteLogarithm cg primRoot cardPubKey, discreteLogarithm cg primRoot doorPubKey)

{-

cardPubKey = transform cardLoopSize 7
doorPubKey = transform doorLoopSize 7
encKey = transform cardLoopSize doorPubKey = transform doorLoopSize cardPubKey

transform loopSize subject = (subject ^ loopSize) mod 20201227
cardPubKey = (7 ^ cardLoopSize) mod 20201227
doorPubKey = (7 ^ doorLoopSize) mod 20201227
encKey
  = ((7 ^ doorLoopSize) ^ cardLoopSize) mod 20201227
  = ((7 ^ cardLoopSize) ^ doorLoopSize) mod 20201227

encKey = transform (cardLoopSize * doorLoopSize) 7
-}

answer1 :: SomeMod
answer1 = powSomeMod (7 `modulo` 20201227) (fst loopSizes * snd loopSizes)