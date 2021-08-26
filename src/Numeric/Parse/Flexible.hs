{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Flexible numeric parsers for real-world programming languages.
-- These parsers aim to be a superset of the numeric syntaxes across
-- the most popular programming languages.
--
-- All parsers assume any trailing whitespace has already been
-- consumed, and places no requirement for an @endOfInput@ at the end
-- of a literal. Be sure to handle these in a calling context. These
-- parsers do not use 'Text.Parser.Token.TokenParsing', and therefore
-- may fail while consuming input, depending on if you use a parser
-- that automatically backtracks or not. Apply 'try' if needed.
module Numeric.Parse.Flexible
  ( integer,
    natural,
    decimal,
    hexadecimal,
    octal,
    binary,
    floating,
    signed,
    imaginary,
  )
where

import Control.Applicative
import Control.Monad hiding (fail)
import Data.Scientific hiding (scientific)
import Numeric
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Read (readMaybe)
import Prelude hiding (exponent, fail, takeWhile)
import Data.Complex
import Numeric.Natural (Natural)

-- | Parse an integer in 'decimal', 'hexadecimal', 'octal', or 'binary', with optional leading sign.
--
-- Note that because the 'octal' parser takes primacy over 'decimal', numbers with a leading
-- @0@ will be parsed as octal. This is unfortunate, but matches the behavior
-- of C, Python, and Ruby.
integer :: (CharParsing m, Monad m) => m Integer
integer = signed (choice [try hexadecimal, try octal, try binary, decimal])

-- | Parse a natural number in 'decimal', 'hexadecimal', 'octal', or 'binary'. As with 'integer',
-- a leading @0@ is interpreted as octal. Leading signs are not accepted.
natural :: (CharParsing m, Monad m) => m Natural
natural = fromIntegral <$> choice [try hexadecimal, try octal, try binary, decimal]

-- | Parse an integer in base 10.
--
-- Accepts @0..9@ and underscore separators. No leading signs are accepted.
decimal :: (CharParsing m, Monad m) => m Integer
decimal = do
  contents <- withUnder digit
  attempt contents

-- | Parse a number in hexadecimal.
--
-- Requires a @0x@ or @0X@ prefix. No leading signs are accepted.
-- Accepts @A..F@, @a..f@, @0..9@ and underscore separators.
hexadecimal :: forall a m. (Eq a, Num a, CharParsing m, Monad m) => m a
hexadecimal = do
  void (string "0x" <|> string "0X")
  contents <- withUnder hexDigit
  let res = readHex contents
  case res of
    [] -> unexpected ("unparsable hex literal " <> contents)
    [(x, "")] -> pure x
    _ -> unexpected ("ambiguous hex literal " <> contents)

-- | Parse a number in octal.
--
-- Requires a @0@, @0o@ or @0O@ prefix. No leading signs are accepted.
-- Accepts @0..7@ and underscore separators.
octal :: forall a m. (Num a, CharParsing m, Monad m) => m a
octal = do
  void (char '0' *> optional (oneOf "oO"))
  digs <- withUnder octDigit
  fromIntegral <$> attempt @Integer ("0o" <> digs)

-- | Parse a number in binary.
--
-- Requires a @0b@ or @0B@ prefix. No leading signs are accepted.
-- Accepts @0@, @1@, and underscore separators.
binary :: forall a m. (Show a, Num a, CharParsing m, Monad m) => m a
binary = do
  void (char '0')
  void (optional (oneOf "bB"))
  digs <- withUnder (oneOf "01")
  let c2b c = case c of
        '0' -> 0
        '1' -> 1
        x -> error ("Invariant violated: both Attoparsec and readInt let a bad digit through: " <> [x])
  let res = readInt 2 (`elem` "01") c2b digs
  case res of
    [] -> unexpected ("No parse of binary literal: " <> digs)
    [(x, "")] -> pure x
    others -> unexpected ("Too many parses of binary literal: " <> show others)

-- | Parse an arbitrary-precision number with an optional decimal part.
--
-- Unlike 'scientificP' or Scientific's 'Read' instance, this handles:
--
--   * omitted whole parts, e.g. @.5@
--   * omitted decimal parts, e.g. @5.@
--   * exponential notation, e.g. @3.14e+1@
--   * numeric parts, in whole or decimal or exponent parts, with @_@ characters
--   * hexadecimal, octal, and binary integer literals, without a decimal part.
--
-- You may either omit the whole or the leading part, not both; this parser also rejects the empty string.
-- It does /not/ handle hexadecimal floating-point numbers.
floating :: (CharParsing m, Monad m) => m Scientific
floating = signed (choice [hexadecimal, octal, binary, dec])
  where
    -- Compared to the binary parser, this is positively breezy.
    dec = do
      -- Try getting the whole part of a floating literal.
      leadings <- stripUnder <$> many (digit <|> char '_')
      -- Try reading a dot.
      void (optional (char '.'))
      -- The trailing part...
      trailings <- stripUnder <$> many (digit <|> char '_')
      -- ...and the exponent.
      exponent <- stripUnder <$> many (oneOf "eE_0123456789+-")
      -- Ensure we don't read an empty string, or one consisting only of a dot and/or an exponent.
      when (null trailings && null leadings) (unexpected "Does not accept a single dot")
      -- Replace empty parts with a zero.
      let leads = if null leadings then "0" else leadings
      let trail = if null trailings then "0" else trailings
      attempt (leads <> "." <> trail <> exponent)

-- | Converts a numeric parser to one that accepts an optional leading sign.
signed :: forall a m . (CharParsing m, Num a) => m a -> m a
signed p =
  (negate <$> (char '-' *> p))
    <|> (char '+' *> p)
    <|> p

-- | Converts a numeric parser to one that accepts a trailing imaginary specifier
-- @i@ or @j@. This does not add facilities for two-valued literals, i.e. @1+4j@,
-- as those are generally best left to high-level expression facilities.
imaginary :: forall a m . (CharParsing m, Monad m, Num a) => m a -> m (Complex a)
imaginary num = do
  real <- num
  void (oneOf "ij")
  pure (0 :+ real)

stripUnder :: String -> String
stripUnder = Prelude.filter (/= '_')

attempt :: (Read a, CharParsing m) => String -> m a
attempt str = maybe (unexpected ("No parse: " <> str)) pure (readMaybe str)

withUnder :: CharParsing m => m Char -> m String
withUnder p = stripUnder <$> ((:) <$> p <*> many (p <|> char '_'))
