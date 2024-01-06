{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Day20 where

import Data.Void (Void)
import Data.Text (Text)
import Data.Bifunctor (first)
import Text.Megaparsec (Parsec, (<|>), errorBundlePretty, runParser, eof, many, choice, sepBy)
import Text.Megaparsec.Char (char, alphaNumChar, space, string)
import Text.Megaparsec.Char.Lexer ()


data ModType
  = Cast -- Broadcast
  | Flip -- Flip-Flop
  | Conj -- Conjuntion
  deriving Show

data Mod = Mod
  { mName :: !String
  , mType :: !ModType
  , mNext :: ![String]
  } deriving Show

type Parser = Parsec Void Text

a :: Text
a =
  "broadcaster -> a, b, c\n\
  \%a -> b\n\
  \%b -> c\n\
  \%c -> inv\n\
  \&inv -> a\n"

b :: Text
b =
  "broadcaster -> a\n\
  \%a -> inv, con\n\
  \&inv -> b\n\
  \%b -> con\n\
  \&con -> output\n"

parse :: Text -> Either String [Mod]
parse = first errorBundlePretty . runParser ms ""
  where
    m :: Parser Mod
    m = do
      mType <- choice [ Flip <$ char '%', Conj <$ char '&' ] <|> pure Cast
      mName <- many alphaNumChar <* space <* string "->" <* space
      mNext <- (many alphaNumChar `sepBy` string ", ") <* space
      return $ Mod {..}
    ms :: Parser [Mod]
    ms = many m <* eof

-- >>> parse a
-- Right [Mod {mName = "broadcaster", mType = Cast, mNext = ["a","b","c"]},Mod {mName = "a", mType = Flip, mNext = ["b"]},Mod {mName = "b", mType = Flip, mNext = ["c"]},Mod {mName = "c", mType = Flip, mNext = ["inv"]},Mod {mName = "inv", mType = Conj, mNext = ["a"]}]

