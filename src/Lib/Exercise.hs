{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Exercise where

import Options.Generic

data Exercise = ES1 FilePath | None
    deriving (Generic, Show)

instance ParseRecord Exercise