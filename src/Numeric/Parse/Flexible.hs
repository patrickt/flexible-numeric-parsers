{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Numeric.Parse.Flexible
  ( scientific,
    integer,
  )
where

import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Attoparsec.Text hiding (scientific)
import Data.Char (isDigit, isHexDigit, isOctDigit)
import Data.Scientific hiding (scientific)
import Data.Text hiding (takeWhile)
import Numeric
import Text.Read (readMaybe)
import Prelude hiding (exponent, fail, filter, null, takeWhile)

-- The ending stanza. Note the explicit endOfInput call to ensure we haven't left any dangling input.
lengths :: Parser ()
lengths = skipWhile (inClass "iIjJlL") *> endOfInput

stripUnder :: Text -> Text
stripUnder = filter (/= '_')

-- Parse a hex value, leaning on the parser provided by Attoparsec.
hex :: Num a => Parser a
hex = do
  void (char '0')
  skip (inClass "xX")
  let isHex c = isHexDigit c || (c == '_')
  contents <- stripUnder <$> takeWhile1 isHex
  let go = fromIntegral <$> hexadecimal @Integer <* lengths
  either fail pure (parseOnly go contents)

-- We lean on Haskell's octal integer support, parsing
-- the given string as an integer then coercing it to a Scientific.
oct :: Num a => Parser a
oct = do
  void (char '0')
  skipWhile (inClass "Oo")
  let isOct c = isOctDigit c || c == '_'
  digs <- stripUnder <$> (takeWhile1 isOct <* lengths)
  fromIntegral <$> attempt @Integer (unpack ("0o" <> digs)) <* lengths

-- The case for binary literals is somewhat baroque. Despite having binary literal support, Integer's
-- Read instance does not handle binary literals. So we have to shell out to Numeric.readInt, which
-- is a very strange API, but works for our use case. The use of 'error' looks partial, but if Attoparsec
-- and readInt do their jobs, it should never happen.
bin :: (Show a, Num a) => Parser a
bin = do
  void (char '0')
  skip (inClass "bB")
  let isBin = inClass "01_"
  digs <- unpack . stripUnder <$> (takeWhile1 isBin <* lengths)
  let c2b c = case c of
        '0' -> 0
        '1' -> 1
        x -> error ("Invariant violated: both Attoparsec and readInt let a bad digit through: " <> [x])
  let res = readInt 2 isBin c2b digs
  case res of
    [] -> fail ("No parse of binary literal: " <> digs)
    [(x, "")] -> x <$ lengths
    others -> fail ("Too many parses of binary literal: " <> show others)

-- Wrapper around readMaybe.
attempt :: Read a => String -> Parser a
attempt str = maybe (fail ("No parse: " <> str)) pure (readMaybe str)

-- | This is a very flexible and forgiving parser for Scientific values.
-- Unlike 'scientificP' or Scientific's 'Read' instance, this handles the myriad
-- array of floating-point syntaxes across languages:
-- * omitted whole parts, e.g. @.5@
-- * omitted decimal parts, e.g. @5.@
-- * numbers with trailing imaginary/length specifiers, @1.7j, 20L@
-- * numeric parts, in whole or decimal or exponent parts, with @_@ characters
-- * hexadecimal, octal, and binary literals (TypeScript needs this because all numbers are floats)
-- You may either omit the whole or the leading part, not both; this parser also rejects the empty string.
-- It does /not/ handle hexadecimal floating-point numbers yet, as no language we parse supports them.
-- This will need to be changed when we support Java.
-- Please note there are extant parser bugs where complex literals (e.g. @123j@) are parsed
-- as floating-point rather than complex quantities. This parser discards all suffixes.
-- This parser is unit-tested in Data.Scientific.Spec.
scientific :: Parser Scientific
scientific = signed (choice [hex, oct, bin, dec])
  where
    -- Compared to the binary parser, this is positively breezy.
    dec = do
      let notUnder = filter (/= '_')
      let decOrUnder c = isDigit c || (c == '_')
      -- Try getting the whole part of a floating literal.
      leadings <- notUnder <$> takeWhile decOrUnder
      -- Try reading a dot.
      void (optional (char '.'))
      -- The trailing part...
      trailings <- notUnder <$> takeWhile decOrUnder
      -- ...and the exponent.
      exponent <- notUnder <$> takeWhile (inClass "eE_0123456789+-")
      lengths
      -- Ensure we don't read an empty string, or one consisting only of a dot and/or an exponent.
      when (null trailings && null leadings) (fail "Does not accept a single dot")
      -- Replace empty parts with a zero.
      let leads = if null leadings then "0" else leadings
      let trail = if null trailings then "0" else trailings
      attempt (unpack (leads <> "." <> trail <> exponent))

-- | As 'scientific', but eliding the ability to provide a floating-point component.
integer :: Parser Integer
integer = signed (choice [hex, oct, bin, dec])
  where
    dec = do
      let notUnder = filter (/= '_')
      let decOrUnder c = isDigit c || (c == '_')
      contents <- notUnder <$> takeWhile1 decOrUnder
      void lengths
      attempt (unpack contents)
