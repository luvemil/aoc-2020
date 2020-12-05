module Lib.StaticInfo (StaticInfo (..)) where

import Data.ByteString.Char8 (ByteString)

data StaticInfo = StaticInfo
    { sVersion :: ByteString
    }
