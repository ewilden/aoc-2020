{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day01 where

import Import
import qualified RIO.HashMap as HashMap
import RIO.List (nub)
import RIO.Partial (read)
import qualified RIO.Text as Text

input :: IO [Int]
input = do
  rawInput <- readFileUtf8 "input/01.txt"
  return $ map (read . Text.unpack) $ Text.linesCR rawInput

answer1 :: [Int] -> [Int]
answer1 inp = do
  let indexedInput :: [(Int, Int)] = zip inp [0 ..]
  (x, ix) <- indexedInput
  (y, iy) <- indexedInput
  if ix == iy || (x + y) /= 2020
    then []
    else return (x * y)

answer2 :: [Int] -> [Int]
answer2 inp = do
  let indexedInput :: [(Int, Int)] = zip inp [0 ..]
  (x, ix) <- indexedInput
  (y, iy) <- indexedInput
  (z, iz) <- indexedInput
  if length [ix, iy, iz] /= length (nub [ix, iy, iz]) || (x + y + z) /= 2020
    then []
    else return (x * y * z)

answer2' :: [Int] -> [Int]
answer2' inp = do
  let indexedInput = zip inp [0 :: Int ..]
      numToIndices = HashMap.fromListWith (<>) $ map (second (: [])) indexedInput
  (x, ix) <- indexedInput
  (y, iy) <- indexedInput
  let z = 2020 - x - y
      zInds = HashMap.lookupDefault [] z numToIndices
  if any (\iz -> iz /= ix && iz /= iy) zInds then return (x * y * z) else []
