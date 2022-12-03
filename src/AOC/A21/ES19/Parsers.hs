module AOC.A21.ES19.Parsers where

import AOC.Utils
import AOC.A21.ES19.AbstractObjects
import AOC.A21.ES19.Geometry
import Text.Megaparsec.Char (char, digitChar)
import Text.Megaparsec (optional, try, many)
import Text.Read (readMaybe)
import Control.Lens
import AOC.A21.ES19.Objects (CBeacon)

coordParser :: Parser Int
coordParser = do
    sign <- optional . try $ char '-'
    n :: Int <- embedMaybe . readMaybe =<< many digitChar
    case sign of
      Nothing -> pure n
      Just _ -> pure $ -n

beaconParser :: Parser CBeacon
beaconParser = do
    pX <- coordParser <* char ','
    pY <- coordParser <* char ','
    pZ <- coordParser
    let p = (pX, pY, pZ) ^. from pointVector
    pure $ Beacon p
