{-# LANGUAGE TemplateHaskell #-}
module AOC.A21.ES19.AbstractObjects where

import Control.Lens

data Scanner p q = Scanner
  { scannerPos :: !p
  , scannerId :: !Int
  , scannerAlign :: !q
  }
  deriving (Eq)

makeLenses ''Scanner

data Beacon p = Beacon
  { beaconPos :: p
  }
  deriving (Eq)

makeLenses ''Beacon
