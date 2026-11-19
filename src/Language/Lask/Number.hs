{-# LANGUAGE OverloadedStrings #-}

-- | The range of @Number@ (spec 4.1): the magnitudes of a finite
-- IEEE 754 double. A number above it is refused where it enters a
-- program, and one too small for it to tell from zero is zero, so that
-- every number has a double, and a text of bounded length.
module Language.Lask.Number
  ( fitNumber,
    maxNumber,
    minNumber,
    numberText,
  )
where

import Data.Scientific (FPFormat (Generic), Scientific, formatScientific, fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as T

-- $setup
-- >>> import Data.Scientific (scientific)

-- | The number as a Number, or 'Nothing' when its magnitude is above
-- 'maxNumber'. A nonzero magnitude below 'minNumber' is 0. Comparing
-- a 'Scientific' goes by its exponent first, so this is cheap even for
-- @1e1000000000@.
--
-- >>> fitNumber (scientific 1 1000000000)
-- Nothing
-- >>> fitNumber 1e308
-- Just 1.0e308
-- >>> fitNumber 1e309
-- Nothing
-- >>> fitNumber (-1e309)
-- Nothing
-- >>> fitNumber 1e-400
-- Just 0.0
-- >>> fitNumber 0.5
-- Just 0.5
fitNumber :: Scientific -> Maybe Scientific
fitNumber n
  | abs n > maxNumber = Nothing
  | n /= 0 && abs n < minNumber = Just 0
  | otherwise = Just n

-- | The largest finite double, to its shortest decimal: about 1.8e308.
maxNumber :: Scientific
maxNumber = fromFloatDigits (1.7976931348623157e308 :: Double)

-- | The smallest positive double, to its shortest decimal: 5e-324.
minNumber :: Scientific
minNumber = fromFloatDigits (5.0e-324 :: Double)

-- | A number as a message shows it, in exponent form when it is large
-- or small, so that the text stays short whatever the number.
--
-- >>> numberText (scientific 1 1000000000)
-- "1.0e1000000000"
numberText :: Scientific -> Text
numberText = T.pack . formatScientific Generic Nothing
