{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day09 where

import Import
import Lens.Micro.Platform
import RIO.HashSet hiding (null)
import RIO.List (headMaybe)
import RIO.List.Partial (maximum, minimum)
import RIO.Partial (read)
import RIO.State
import qualified RIO.Text as Text

input :: IO [Int]
input = do
  rawInput <- readFileUtf8 "input/09.txt"
  return $ read . Text.unpack <$> Text.lines rawInput

data Zipper a = Zipper
  { _backward :: ![a],
    _forward :: ![a]
  }

makeLenses ''Zipper

advance :: State (Zipper Int) (Either () ())
advance = do
  fwdList <- forward <<%= drop 1
  case fwdList of
    [] -> return $ Left ()
    (next : _) -> do
      backward %= (next :)
      return $ Right ()

firstInvalid :: ([Int] -> Int -> Bool) -> State (Zipper Int) (Maybe Int)
firstInvalid isValid = do
  lookback <- use $ backward . to (take 25)
  mayNext <- use $ forward . to headMaybe
  case mayNext of
    Nothing -> return Nothing
    Just next -> do
      if length lookback < 25
        then do
          advance
          firstInvalid isValid
        else
          if not (isValid lookback next)
            then return $ Just $ next
            else do
              advance
              firstInvalid isValid

isValid1 :: [Int] -> Int -> Bool
isValid1 lookback next =
  let lookbackSet = fromList lookback
   in not $
        null $ do
          x <- lookback
          if x * 2 /= next && (next - x) `member` lookbackSet then [(x, next - x)] else []

answer1 :: [Int] -> Maybe Int
answer1 inp = evalState (firstInvalid isValid1) (Zipper [] inp)

findSublistsSummingTo :: [Int] -> Int -> [[Int]]
findSublistsSummingTo ls num = do
  startIndex <- [0 .. length ls]
  endIndex <- [startIndex + 2 .. length ls]
  let sublist = ls & take endIndex & drop startIndex
  if sum sublist == num then return sublist else []

answer2 :: [Int] -> [Int]
answer2 inp =
  findSublistsSummingTo inp (fromMaybe undefined $ answer1 inp)
    & fmap (\sublist -> minimum sublist + maximum sublist)