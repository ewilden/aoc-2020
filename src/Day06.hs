{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day06 where

import Import
import qualified RIO.HashSet as HashSet
import RIO.List.Partial (maximum, minimum)
import qualified RIO.Text as Text
import RIO.Text.Partial (splitOn)

input :: IO [[Text]]
input = do
  rawInput <- readFileUtf8 "input/06.txt"
  return $ map Text.lines $ splitOn "\n\n" rawInput

processGroup :: [Text] -> HashSet Char
processGroup answers = foldl' HashSet.union mempty answerSets
  where
    answerSets = map (HashSet.fromList . Text.unpack) answers

answer1 :: [[Text]] -> Int
answer1 = sum . map (HashSet.size . processGroup)

processGroup2 :: [Text] -> HashSet Char
processGroup2 answers = foldl' HashSet.intersection (HashSet.fromList ['a' .. 'z']) answerSets
  where
    answerSets = map (HashSet.fromList . Text.unpack) answers

answer2 :: [[Text]] -> Int
answer2 = sum . map (HashSet.size . processGroup2)