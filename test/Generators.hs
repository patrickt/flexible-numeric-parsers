{-# LANGUAGE OverloadedStrings #-}

module Generators
  ( module Generators,
  )
where

import Data.Ratio ((%))
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import Hedgehog
import qualified Hedgehog.Gen as Gen

integerScientific :: MonadGen m => Hedgehog.Range Integer -> m Scientific
integerScientific = fmap fromIntegral . Gen.integral

rationalScientific :: MonadGen m => Hedgehog.Range Integer -> Hedgehog.Range Integer -> m Scientific
rationalScientific nrange drange = do
  num <- Gen.integral nrange
  den <- Gen.integral drange
  let goodDen = if den == 0 then 1 else den
  let digitLimit = Just 25
  case Scientific.fromRationalRepetend digitLimit (num % goodDen) of
    Left (sci, _) -> pure sci
    Right (sci, _) -> pure sci

floatingScientific :: MonadGen m => Hedgehog.Range Double -> m Scientific
floatingScientific = fmap Scientific.fromFloatDigits . Gen.double

classifyScientific :: MonadTest m => Scientific -> m ()
classifyScientific sci = do
  classify "negative" $ sci < 0
  classify "small" $ (sci > 0 && sci <= 1)
  classify "medium" $ (sci > 1 && sci <= 10000)
  classify "large" $ sci > 10000
