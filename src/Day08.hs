{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day08 where

import Import
import Lens.Micro.Platform
import RIO.HashSet
import RIO.Partial (read)
import RIO.State
import qualified RIO.Text as Text

data InstrType = Acc | Jmp | Nop deriving (Eq, Ord, Show)

type Instr = (InstrType, Int)

input :: IO [Instr]
input = do
  rawInput <- readFileUtf8 "input/08.txt"
  return $ fmap parseInstr $ Text.lines rawInput
  where
    parseInstr :: Text -> Instr
    parseInstr txt =
      let [rawOp, rawSignedNum] = Text.words txt
          instrType = case rawOp of
            "nop" -> Nop
            "jmp" -> Jmp
            "acc" -> Acc
          sign :: Int
          sign = if Text.take 1 rawSignedNum == "-" then -1 else 1
          num :: Int
          num = read (drop 1 $ Text.unpack rawSignedNum)
       in (instrType, sign * num)

data PgmState = PgmState
  { _pc :: Int,
    _seenPcs :: HashSet Int,
    _acc :: Int
  }
  deriving (Show)

makeLenses ''PgmState

data Termination = Loop Int | End Int deriving (Show)

initialState :: PgmState
initialState = PgmState 0 mempty 0

runPgm :: [Instr] -> State PgmState Termination
runPgm instrs = do
  currPc <- use pc
  seen <- use seenPcs
  if currPc `member` seen
    then Loop <$> use acc
    else do
      seenPcs %= insert currPc
      case instrs ^? ix currPc of
        Nothing -> End <$> use acc
        Just (instrType, num) -> do
          case instrType of
            Nop -> pc += 1
            Jmp -> pc += num
            Acc -> do acc += num; pc += 1
          runPgm instrs

answer1 :: [Instr] -> Termination
answer1 inp = evalState (runPgm inp) initialState

swap :: InstrType -> InstrType
swap Jmp = Nop
swap Nop = Jmp
swap Acc = Acc

answer2 :: [Instr] -> [Termination]
answer2 instrs = do
  idxToPatch <- [0 .. (length instrs - 1)]
  case instrs ^?! ix idxToPatch of
    (Acc, _) -> []
    (op, _) -> case answer1 (set (ix idxToPatch . _1) (swap op) instrs) of
      Loop _ -> []
      result -> [result]
