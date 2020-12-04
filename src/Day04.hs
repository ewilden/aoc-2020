{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day04 where

import Import hiding (lookup, many, try)
import RIO.HashMap (lookup)
import qualified RIO.HashMap as HashMap
import qualified RIO.Text as Text
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

data FieldType
  = Byr
  | Iyr
  | Eyr
  | Hgt
  | Hcl
  | Ecl
  | Pid
  | Cid
  deriving (Show, Eq, Ord, Enum, Generic)

instance Hashable FieldType

type Field = (FieldType, Text)

type Passport = [Field]

type Parser = Parsec Void Text

pFieldType :: Parser FieldType
pFieldType =
  string "byr" $> Byr
    <|> string "iyr" $> Iyr
    <|> string "eyr" $> Eyr
    <|> string "hgt" $> Hgt
    <|> string "hcl" $> Hcl
    <|> string "ecl" $> Ecl
    <|> string "pid" $> Pid
    <|> string "cid" $> Cid

pField :: Parser Field
pField = do
  fieldType <- pFieldType
  _ <- char ':'
  txt <- takeWhile1P (Just "non-whitespace") (\c -> c /= ' ' && c /= '\n')
  return (fieldType, txt)

pNonemptyList :: Parser a -> Parser b -> Parser [a]
pNonemptyList pElem pSep = do
  h <- pElem
  tl <- many $ try $ pSep *> pElem
  return $ h : tl

pPassport :: Parser Passport
pPassport = pNonemptyList pField (char ' ' <|> char '\n')

pInput :: Parser [Passport]
pInput = pNonemptyList pPassport (string "\n\n")

input :: IO [Passport]
input = do
  rawInput <- readFileUtf8 "input/04.txt"
  case runParser pInput "Day 4 input" rawInput of
    Left err -> error $ show err
    Right inp -> return inp

isValid1 :: Passport -> Bool
isValid1 fields =
  let fieldTypes = map fst fields
   in length fieldTypes == 8
        || length fieldTypes == 7 && not (Cid `elem` fieldTypes)

answer1 :: [Passport] -> Int
answer1 inp = length $ filter isValid1 inp

data HeightUnit = Cm | In

pHeight :: Parser (Int, HeightUnit)
pHeight = do
  num <- decimal
  unit <- (string "cm" $> Cm) <|> (string "in" $> In)
  return (num, unit)

isInRange :: Int -> (Int, Int) -> Bool
isInRange i (lo, hi) = i >= lo && i <= hi

pHairColor :: Parser ()
pHairColor = do
  _ <- char '#'
  _ <- count 6 $ oneOf (['0' .. '9'] ++ ['a' .. 'f'])
  return ()

pEyeColor :: Parser ()
pEyeColor = () <$ choice (map string ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

pPassportId :: Parser ()
pPassportId = () <$ count 9 (oneOf ['0' .. '9'])

isValid2 :: Passport -> Bool
isValid2 passport
  | not (isValid1 passport) = False
  | otherwise =
    let fields = Cid `HashMap.delete` HashMap.fromList passport
     in maybe False id $ do
          let check b = if b then pure () else Nothing
          let intField :: FieldType -> Maybe Int
              intField t = readMaybe . Text.unpack =<< t `lookup` fields
          byr <- intField Byr
          check $ byr `isInRange` (1920, 2002)
          iyr <- intField Iyr
          check $ iyr `isInRange` (2010, 2020)
          eyr <- intField Eyr
          check $ eyr `isInRange` (2020, 2030)
          hgt :: (Int, HeightUnit) <- Hgt `lookup` fields >>= parseMaybe pHeight
          check $ case snd hgt of
            Cm -> fst hgt `isInRange` (150, 193)
            In -> fst hgt `isInRange` (59, 76)
          Hcl `lookup` fields >>= parseMaybe pHairColor
          Ecl `lookup` fields >>= parseMaybe pEyeColor
          Pid `lookup` fields >>= parseMaybe pPassportId
          return True

answer2 :: [Passport] -> Int
answer2 = length . filter isValid2