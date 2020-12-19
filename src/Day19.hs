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

module Day19 where

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
import RIO.List (headMaybe, iterate, sort, takeWhile)
import RIO.List.Partial (head, last, maximum, minimum, minimumBy, (!!))
import qualified RIO.Map as M
import RIO.Partial (fromJust, read)
import RIO.State
import qualified RIO.Text as Text
import RIO.Text.Partial (splitOn)
import qualified RIO.Vector.Boxed as VB
import RIO.Writer
import Text.ParserCombinators.ReadP

input :: String -> IO ([Text], [Text])
input filename = do
  rawInput <- readFileUtf8 filename
  let [rules, messages] = splitOn "\n\n" rawInput
  return (Text.lines rules, Text.lines messages)

type Parser = ReadP

newtype ParseCtx = ParseCtx
  { unParseCtx :: HashMap Text (ParseCtx -> Parser ())
  }

parseRule :: Text -> (Text, ParseCtx -> Parser ())
parseRule rawRule =
  let [key, ruleBody] = splitOn ": " (rawRule)
   in ( key,
        \ctx@(ParseCtx parserMap) -> do
          let orOfSeqs = splitOn " | " (ruleBody) <&> Text.words
              toParser :: Text -> Parser ()
              toParser parserName = case Text.unpack (parserName) of
                '"' : c : '"' : [] -> void $ char c
                _ -> fromMaybe undefined (parserName `HM.lookup` parserMap) ctx
              subparserList = (fmap (mapM_ toParser) orOfSeqs)
          choice subparserList
      )

parseRules :: [Text] -> ParseCtx
parseRules rawRules = ParseCtx $ HM.fromList (fmap parseRule rawRules)

runParser :: ReadP a -> b -> Text -> Either () ()
runParser parser _ t = if null (readP_to_S parser (Text.unpack t)) then Left () else Right ()

answer :: ([Text], [Text]) -> Int
answer (rawRules, rawMessages) =
  let parseCtx = parseRules rawRules
      ruleZero = HM.lookupDefault undefined "0" (unParseCtx parseCtx) parseCtx
      didMessageParse = fmap (isRight . runParser (ruleZero >> eof) "message") rawMessages
   in length $ filter id didMessageParse

answer1 = answer <$> input "input/19.txt"

answer2 = answer <$> input "input/19-2.txt"

rightPlus :: Parser a -> Parser a -> Parser a
rightPlus = flip (<|>)

-- answer2' :: ([Text], [Text]) -> Int
-- answer2' (rawRules, rawMessages) =
--   let pctx@(ParseCtx parserMap) = parseRules rawRules
--       p42 = HM.lookupDefault undefined "42" parserMap pctx
--       p31 = HM.lookupDefault undefined "31" parserMap pctx
--       -- p8 = try $ void $ some p42
--       -- p11 = try $ do
--       --   fortyTwos <- some p42
--       --   void $ count (length fortyTwos) p31
--       p0 = do
--         fortyTwos <- some p42
--         thirtyOnes <- some p31
--         when (length fortyTwos < length thirtyOnes) $ failure Nothing mempty
--       -- parserMap' = HM.insert "8" (const p8) (HM.insert "11" (const p11) parserMap)
--       -- -- parseCtx = ParseCtx parserMap'
--       -- ruleZero = HM.lookupDefault undefined "0" (unParseCtx parseCtx) parseCtx
--       didMessageParse = fmap (isRight . runParser (p0 >> eof) "message") rawMessages
--    in length $ filter id didMessageParse