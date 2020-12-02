{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day02 where

import Import
import Lens.Micro.Platform
import RIO.Partial (read)
import qualified RIO.Text as Text

data PasswordEntry = PasswordEntry
  { _policy :: ((Int, Int), Char),
    _password :: String
  }

parsePasswordEntry :: Text -> PasswordEntry
parsePasswordEntry line =
  let [rawRange, [char, ':'], password] = words $ Text.unpack line
      (rawMin, '-' : rawMax) = break (== '-') rawRange
   in PasswordEntry ((read rawMin, read rawMax), char) password

input :: IO [PasswordEntry]
input = do
  map parsePasswordEntry . Text.lines <$> readFileUtf8 "input/02.txt"

isValid1 :: PasswordEntry -> Bool
isValid1 (PasswordEntry ((lo, hi), char) password) = lo <= count && count <= hi
  where
    count = length $ filter (== char) password

answer1 :: [PasswordEntry] -> Int
answer1 inp = length $ filter isValid1 inp

isValid2 :: PasswordEntry -> Bool
isValid2 (PasswordEntry ((lo, hi), char) password) = checkInd lo /= checkInd hi
  where
    checkInd ind = password ^? ix (ind - 1) == Just char

answer2 :: [PasswordEntry] -> Int
answer2 inp = length $ filter isValid2 inp