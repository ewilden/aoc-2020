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

module Day18 where

-- import Debug.Trace

import Control.Monad (replicateM)
import Control.Monad.Combinators.Expr
import qualified Data.Bimap as Bimap
import Data.Bits
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Semigroup (Sum (..))
import Data.Tuple
import Import
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
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = Lex.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = Lex.symbol spaceConsumer

data Expr
  = Lit Int
  | Add Expr Expr
  | Mult Expr Expr
  deriving (Show, Eq)

add :: Parser (Expr -> Expr -> Expr)
add = Add <$ symbol "+"

mult :: Parser (Expr -> Expr -> Expr)
mult = Mult <$ symbol "*"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Expr
integer = Lit <$> lexeme Lex.decimal

term :: Parser Expr
term = parens expr <|> integer

expr :: Parser Expr
expr = makeExprParser term [[InfixL add, InfixL mult]]

input :: IO [Expr]
input = do
  rawInput <- readFileUtf8 "input/18.txt"
  return $
    Text.lines rawInput <&> runParser expr "expr" <&> \case
      Left e -> error (show e)
      Right r -> r

compute :: Expr -> Int
compute (Lit i) = i
compute (Add l r) = (compute l) + (compute r)
compute (Mult l r) = (compute l) * (compute r)

answer1 :: [Expr] -> Int
answer1 inp = sum $ fmap compute inp

term2 :: Parser Expr
term2 = parens expr2 <|> integer

expr2 :: Parser Expr
expr2 = makeExprParser term2 [[InfixL add], [InfixL mult]]

input2 :: IO [Expr]
input2 = do
  rawInput <- readFileUtf8 "input/18.txt"
  return $
    Text.lines rawInput <&> runParser expr2 "expr2" <&> \case
      Left e -> error (show e)
      Right r -> r

answer2 :: [Expr] -> Int
answer2 inp = compute $ foldr Add (Lit 0) inp