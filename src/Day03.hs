{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day03 where

import Import
import RIO.List (cycle)
import qualified RIO.Text as Text

parseLine :: Text -> [Bool]
parseLine line = cycle $ map (== '#') $ Text.unpack line

input :: IO [[Bool]]
input = do
  rawInput <- readFileUtf8 "input/03.txt"
  return $ map parseLine $ Text.lines rawInput

numTreesHit :: [[Bool]] -> Int -> Int -> Int
numTreesHit [] _ _ = 0
numTreesHit inp@((here : _) : _) right down =
  (if here then 1 else 0) + numTreesHit newInp right down
  where
    newInp = inp & drop down & map (drop right)

answer1 :: [[Bool]] -> Int
answer1 inp = numTreesHit inp 3 1

answer2 :: [[Bool]] -> Int
answer2 inp =
  product $
    map
      (uncurry $ numTreesHit inp)
      [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]