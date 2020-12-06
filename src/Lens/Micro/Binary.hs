{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lens.Micro.Binary where

import Import
import Lens.Micro.Internal
import Lens.Micro.Platform
import qualified RIO.HashSet as HashSet

newtype BitSet = BitSet {unBitSet :: HashSet Natural}

bget :: Natural -> BitSet -> Bool
bget i (BitSet s) = i `HashSet.member` s

bput :: Natural -> Bool -> BitSet -> BitSet
bput i b (BitSet s) = BitSet $ (if b then HashSet.insert else HashSet.delete) i s

type instance Index BitSet = Natural

type instance IxValue BitSet = Bool

instance Ixed BitSet where
  ix k f s = f (k `bget` s) <&> \v' -> bput k v' s

type instance Index Natural = Natural

type instance IxValue Natural = Bool

instance Ixed Natural where
  ix k f n = f (n ^?! bitset . ix k) <&> \v' -> n & bitset . ix k .~ v'

bitset :: Lens' Natural BitSet
bitset = lens (toBitset 1) fromBitset
  where
    toBitset _ 0 = BitSet mempty
    toBitset place n = bput (place - 1) (n `mod` 2 == 1) $ toBitset (place + 1) (n `div` 2)
    fromBitset :: Natural -> BitSet -> Natural
    fromBitset _ s = fromInteger $ sum $ map (2 ^) $ toList $ unBitSet s

-- fromBitset n bs = fromInteger $ fromIntegral $ length $ toList bs