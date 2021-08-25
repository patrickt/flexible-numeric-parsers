{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

-- | Flexible numeric parsers for real-world programming languages. These parsers aim to be a superset of
-- the numeric syntaxes across the most popular programming languages.
--
-- All parsers assume any trailing whitespace has already been consumed, and places no requirement for an
-- @endOfInput@ at the end of a literal. Be sure to handle these in a calling context.
module Numeric.Parse.Flexible
  ( integer,
    hexadecimal,
    octal,
    binary,
    floating,
  )
where

import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Attoparsec.Text hiding (try, decimal, hexadecimal, scientific, signed, digit)
import Data.Char (isDigit)
import Data.Scientific hiding (scientific)
import Text.Parser.Char (oneOf, hexDigit, octDigit, digit, CharParsing)
import qualified Text.Parser.Char as P
import Text.Parser.Combinators (try, unexpected)
import Data.Text hiding (takeWhile)
import qualified Data.Text as T
import Numeric
import Text.Read (readMaybe)
import Prelude hiding (exponent, fail, null, takeWhile)

-- | Parse an integer in 'decimal', 'hexadecimal', 'octal', or 'binary'.
-- Note that because the 'octal' parser takes primacy, numbers with a leading
-- @0@ will be parsed as octal. This is unfortunate, but matches the behavior
-- of C, Python, and Ruby.
integer :: (CharParsing m, Monad m) => m Integer
integer = signed (choice [try hexadecimal, try octal, try binary, decimal])

-- | Parse an integer in base 10.
-- Accepts @0..9@ and underscore separators.
decimal :: (CharParsing m, Monad m) => m Integer
decimal = do
  contents <- stripUnder' <$> withUnder digit
  attempt contents

-- | Parse a number in hexadecimal.
-- Requires a @0x@ or @0X@ prefix.
-- Accepts @A..F@, @a..f@, @0..9@ and underscore separators.
hexadecimal :: forall a m .(Eq a, Num a, P.CharParsing m, Monad m) => m a
hexadecimal = do
  void (P.string "0x" <|> P.string "0X")
  contents <- stripUnder' <$> withUnder hexDigit
  let res = readHex contents
  case res of
    [] -> unexpected ("unparsable hex literal " <> contents)
    [(x, "")] -> pure x
    _ -> unexpected ("ambiguous hex literal " <> contents)

-- | Parse a number in octal.
-- Requires a @0@, @0o@ or @0O@ prefix.
-- Accepts @0..7@ and underscore separators.
octal :: forall a m . (Num a, CharParsing m, Monad m) => m a
octal = do
  void (P.char '0' *> optional (oneOf "oO"))
  digs <- stripUnder' <$> withUnder octDigit
  fromIntegral <$> attempt @Integer ("0o" <> digs)

-- | Parse a number in binary.
-- Requires a @0b@ or @0B@ prefix.
-- Accepts @0@, @1@, and underscore separators.
binary :: forall a m . (Show a, Num a, CharParsing m, Monad m) => m a
binary = do
  void (P.char '0')
  void (optional (oneOf "bB"))
  let isBin = inClass "01_"
  digs <- stripUnder' <$> withUnder (oneOf "01")
  let c2b c = case c of
        '0' -> 0
        '1' -> 1
        x -> error ("Invariant violated: both Attoparsec and readInt let a bad digit through: " <> [x])
  let res = readInt 2 isBin c2b digs
  case res of
    [] -> unexpected ("No parse of binary literal: " <> digs)
    [(x, "")] -> pure x
    others -> unexpected ("Too many parses of binary literal: " <> show others)

-- | Parse an arbitrary-precision number with an optional decimal part.
-- Unlike 'scientificP' or Scientific's 'Read' instance, this handles:
-- * omitted whole parts, e.g. @.5@
-- * omitted decimal parts, e.g. @5.@
-- * exponential notation, e.g. @3.14e+1@
-- * numeric parts, in whole or decimal or exponent parts, with @_@ characters
-- * hexadecimal, octal, and binary integer literals
-- You may either omit the whole or the leading part, not both; this parser also rejects the empty string.
-- It does /not/ handle hexadecimal floating-point numbers. Nor does it handle
-- as floating-point rather than complex quantities. This parser discards all suffixes.
-- This parser is unit-tested in Data.Scientific.Spec.
floating :: Parser Scientific
floating = signed (choice [hexadecimal, octal, binary, dec])
  where
    -- Compared to the binary parser, this is positively breezy.
    dec = do
      let notUnder = T.filter (/= '_')
      let decOrUnder c = isDigit c || (c == '_')
      -- Try getting the whole part of a floating literal.
      leadings <- notUnder <$> takeWhile decOrUnder
      -- Try reading a dot.
      void (optional (char '.'))
      -- The trailing part...
      trailings <- notUnder <$> takeWhile decOrUnder
      -- ...and the exponent.
      exponent <- notUnder <$> takeWhile (inClass "eE_0123456789+-")
      -- Ensure we don't read an empty string, or one consisting only of a dot and/or an exponent.
      when (null trailings && null leadings) (fail "Does not accept a single dot")
      -- Replace empty parts with a zero.
      let leads = if null leadings then "0" else leadings
      let trail = if null trailings then "0" else trailings
      attempt (unpack (leads <> "." <> trail <> exponent))

signed :: (CharParsing m, Num a) => m a -> m a
signed p = (negate <$> (P.char '-' *> p))
       <|> (P.char '+' *> p)
       <|> p

stripUnder' :: String -> String
stripUnder' = Prelude.filter (/= '_')

attempt :: (Read a, CharParsing m) => String -> m a
attempt str = maybe (unexpected ("No parse: " <> str)) pure (readMaybe str)

withUnder :: P.CharParsing m => m Char -> m String
withUnder p = (:) <$> p <*> many (p <|> P.char '_')
