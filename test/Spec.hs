{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Data.Attoparsec.Text (parseOnly)
import Data.Either
import Data.Foldable (traverse_)
import Data.Scientific
import Data.Foldable (for_)
import Data.Text as Text
import qualified Generators as Gen
import Hedgehog
import qualified Hedgehog.Range as Range
import Numeric.Parse.Flexible as Flex
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit
import Test.Tasty.HUnit as HUnit
import Test.Tasty.Hedgehog

-- Integer stuff

type IFixture = (Text, Integer)

python :: [IFixture]
python =
  [ ("-1", (negate 1)),
    ("0xDEAD", 0xDEAD),
    ("0XDEAD", 0xDEAD),
    ("1j", 1),
    ("0o123", 83),
    ("0O123", 83),
    ("0b001", 1),
    ("0B001", 1),
    ("1_1", 11), -- underscore syntax is Python 3 only
    ("0B1_1", 3),
    ("0O1_1", 9),
    ("0L", 0)
  ]

ruby :: [IFixture]
ruby =
  [ ("0xa_bcd_ef0_123_456_789", 0xabcdef0123456789),
    ("01234567", 342391)
  ]

integerTestTree :: Tasty.TestTree
integerTestTree =
  let go = traverse_ (\(s, v) -> parseInteger s @?= Right v)
   in Tasty.testGroup
        "Numeric.Exts"
        [ testCase "handles Python integers" (go python),
          testCase "handles Ruby integers" (go ruby),
          testCase "doesn't accept floats" (isLeft (parseInteger "1.5") @? "Accepted floating-point"),
          testCase "doesn't accept empty string" (isLeft (parseInteger "") @? "Accepted integer")
        ]

parseInteger :: Text -> Either String Integer
parseInteger = parseOnly Flex.integer

parseScientific :: Text -> Either String Scientific
parseScientific = parseOnly Flex.scientific

type SFixture = [(Text, Scientific)]

testSFixture :: [(Text, Scientific)] -> HUnit.Assertion
testSFixture vals = for_ vals $ \(input, expected) -> assertEqual (Text.unpack input) (parseScientific input) (Right expected)

pythonSyntax :: SFixture
pythonSyntax =
  [ ("-.6_6", -0.66),
    ("+.1_1", 0.11),
    ("123.4123", 123.4123),
    ("123.123J", 123.123), -- TODO: handle complex values separately in the parser
    ("1_1.3_1", 11.31),
    ("1_1.", 11.0),
    ("99E+01", 99e1),
    ("1e+3_4j", 1e34),
    ("3.e14", 3e14),
    (".3e1_4", 0.3e14),
    ("1_0.l", 10), -- this and the subsequent ones don't actually seem to be valid syntax, we should fix this in tree-sitter
    (".3", 0.3),
    (".1l", 0.1) -- omitting a leading 0 is deprecated in python 3, also note that the -l suffix is not valid in Python 3
  ]

rubySyntax :: SFixture
rubySyntax =
  [ ("1.234_5e1_0", 1.2345e10),
    ("1E30", 1e30),
    ("1.2i", 1.2),
    ("1.0e+6", 1.0e6),
    ("1.0e-6", 1.0e-6)
  ]

jsSyntax :: SFixture
jsSyntax =
  [ ("101", 101),
    ("3.14", 3.14),
    ("3.14e+1", 3.14e1),
    ("0x1ABCDEFabcdef", 470375954370031),
    ("0o7632157312", 1047060170),
    ("0b1010101001", 681)
  ]

testTree :: Tasty.TestTree
testTree =
  Tasty.testGroup
    "Data.Scientific.Exts"
    [ testCase "Python float syntax" $ testSFixture pythonSyntax,
      testCase "Ruby float syntax" $ testSFixture rubySyntax,
      testCase "JavaScript float syntax" $ testSFixture jsSyntax,
      testCase "Pathological input" $ do
        isLeft (parseScientific ".") @? "Accepted period"
        isLeft (parseScientific "") @? "Accepted empty string",
      testProperty "Scientific roundtripping" . property $ do
        let nrange = Range.linear (negate 500000) 20000000
            drange = Range.exponential 1 100000000
        fromRat <- forAll (Gen.rationalScientific nrange drange)
        Gen.classifyScientific fromRat
        tripping fromRat (pack . show) parseScientific,
      testProperty "Double-based Scientific roundtripping" . property $ do
        fromDbl <- forAll (Gen.floatingScientific (Range.linearFrac (negate 1) 3))
        Gen.classifyScientific fromDbl
        tripping fromDbl (pack . show) parseScientific
    ]

main :: IO ()
main = Tasty.defaultMain (Tasty.testGroup "All tests" [testTree, integerTestTree])
