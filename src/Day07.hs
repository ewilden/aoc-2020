{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day07 where

import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Import hiding (lookup, many, some, try)
import Lens.Micro.Platform
import RIO.HashMap (lookup)
import qualified RIO.Map as Map
import RIO.Partial (read)
import qualified RIO.Set as Set
import RIO.State
import RIO.Text (pack)
import qualified RIO.Text as Text
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import Prelude (putStrLn)

type Parser = Parsec Void Text

type Bag = (Text, Text)

type BagSpec = MonoidalMap Bag ContainedBags

type ContainedBags = [(Int, Bag)]

spaceConsumer :: Parser ()
spaceConsumer = Lex.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = Lex.symbol spaceConsumer

pBag :: Parser Bag
pBag = do
  adj <- lexeme $ some letterChar
  color <- lexeme $ some letterChar
  return (pack adj, pack color)

pInputLine :: Parser BagSpec
pInputLine = do
  bag <- pBag
  void $ symbol "bags contain"
  containedBags <-
    try noOtherBags <|> do
      nonLastBags <- mconcat <$> many (try nonLastEntry)
      lastBags <- lastEntry
      return $ nonLastBags <> lastBags
  return $ MMap.singleton bag containedBags

pInput :: Parser BagSpec
pInput = do
  parsedLines <- many pInputLine <* eof
  return $ mconcat parsedLines

noOtherBags :: Parser ContainedBags
noOtherBags = do
  void $ symbol "no other bags."
  return []

nonLastEntry :: Parser ContainedBags
nonLastEntry = do
  rawNum <- lexeme $ some numberChar
  (adj, color) <- pBag
  void $ try (symbol "bags,") <|> symbol "bag,"
  return [(read rawNum, (adj, color))]

lastEntry :: Parser ContainedBags
lastEntry = do
  rawNum <- lexeme $ some numberChar
  (adj, color) <- pBag
  void $ try (symbol "bags.") <|> symbol "bag."
  return [(read rawNum, (adj, color))]

-- parseInput :: Parser BagSpec
-- parseInput = do

pNonemptyList :: Parser a -> Parser b -> Parser [a]
pNonemptyList pElem pSep = do
  h <- pElem
  tl <- many $ try $ pSep *> pElem
  return $ h : tl

input :: IO BagSpec
input = do
  rawInput <- readFileUtf8 "input/07.txt"
  case (runParser pInput "input/07.txt" rawInput) of
    Left err -> error $ show err
    Right result -> return result

invertMap :: BagSpec -> MonoidalMap Bag [Bag]
invertMap origMap =
  foldl'
    ( \mmap (b, bs) ->
        mmap <> MMap.singleton b bs
    )
    mempty
    (concatMap toNewEntries $ MMap.toList origMap)
  where
    toNewEntries :: (Bag, ContainedBags) -> [(Bag, [Bag])]
    toNewEntries (bag, countedBags) = do
      (_, containedBag) <- countedBags
      return (containedBag, [bag])

class HasBag a where
  getBag :: a -> Bag

instance HasBag Bag where
  getBag = id

instance HasBag (Int, Bag) where
  getBag = snd

reachableFrom :: (HasBag a) => Bag -> MonoidalMap Bag [a] -> Set.Set Bag
reachableFrom bag bagMap =
  let getNeighbors :: Bag -> Set.Set Bag
      getNeighbors b = maybe mempty (Set.fromList . map getBag) (MMap.lookup b bagMap)
      bfs :: State (Set.Set Bag, [Bag]) ()
      bfs = do
        (seenSet, workList) <- get
        case workList of
          [] -> pure ()
          (bag : rest) -> do
            _2 .= rest
            if bag `Set.member` seenSet
              then pure ()
              else do
                _1 %= Set.insert bag
                _2 %= (++ Set.toList (getNeighbors bag))
            bfs
   in fst $ execState bfs (mempty, Set.toList (getNeighbors bag))

answer1 :: BagSpec -> Set.Set Bag
answer1 inp = reachableFrom ("shiny", "gold") $ invertMap inp
