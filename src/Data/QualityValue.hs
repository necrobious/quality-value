module Data.QualityValue (parseQValue) where

import Control.Applicative ((<$>))
import Data.Attoparsec.Text (Parser, skipSpace, char, double)

{- | RFC-2616 § 3.9 Quality Values
   HTTP content negotiation (section 12) uses short "floating point" numbers to indicate the relative importance ("weight") of various negotiable parameters. A weight is normalized to a real number in the range 0 through 1, where 0 is the minimum and 1 the maximum value. If a parameter has a quality value of 0, then content with this parameter is `not acceptable' for the client. HTTP/1.1 applications MUST NOT generate more than three digits after the decimal point. User configuration of these values SHOULD also be limited in this fashion.

       qvalue         = ( "0" [ "." 0*3DIGIT ] )
                      | ( "1" [ "." 0*3("0") ] )
   "Quality values" is a misnomer, since these values merely represent relative degradation in desired quality.
-}
parseQValue :: Parser Double
parseQValue = do
  skipSpace
  char ';'
  skipSpace
  char 'q'
  skipSpace
  char '='
  skipSpace
  (loosePrecision 3) <$> normalize <$> double

-- ensure we are always working with a double, between 0.0 and 1.0 
normalize :: Double -> Double
normalize input 
  | input > 1.0 = 1.0
  | input < 0 = normalize (abs input)
  | otherwise = input

-- Drops a Double's precision less than the given precision. 
-- i.e. loosePrecision 2 1.23456 ~> 1.23
loosePrecision :: Int -> Double -> Double
loosePrecision precision input = 
  let p = (10.0::Double) ^ precision
  in  (fromIntegral $ floor (input * p)) / p

