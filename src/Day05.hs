{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day05 where

import Import
import qualified RIO.HashSet as HashSet
import RIO.List.Partial (maximum)
import qualified RIO.Text as Text

input :: IO [Text]
input = do
  rawInput <- readFileUtf8 "input/05.txt"
  return $ Text.lines rawInput

getRowCol :: Text -> (Int, Int)
getRowCol txt = (row, col)
  where
    (rowCode, colCode) = Text.splitAt 7 txt
    row = interpretAsBinary (== 'B') rowCode
    col = interpretAsBinary (== 'R') colCode
    interpretAsBinary is1 = Text.foldl (shiftAndAdd is1) 0
    shiftAndAdd is1 (int :: Int) c =
      let digit = if is1 c then 1 else 0
       in (int * 2) + digit

toId :: Int -> Int -> Int
toId r c = 8 * r + c

answer1 :: [Text] -> Int
answer1 = maximum . map (uncurry toId . getRowCol)

allPossibleSeats :: [(Int, Int)]
allPossibleSeats = (,) <$> [0 .. 127] <*> [0 .. 7]

allPossibleIds :: [Int]
allPossibleIds = map (uncurry toId) allPossibleSeats

answer2 :: [Text] -> [Int]
answer2 inp =
  let takenIds = HashSet.fromList $ map (uncurry toId . getRowCol) inp
      canBeAnswer i
        | i `HashSet.member` takenIds = False
        | (i + 1) `HashSet.member` takenIds = (i - 1) `HashSet.member` takenIds
        | otherwise = False
   in filter canBeAnswer allPossibleIds