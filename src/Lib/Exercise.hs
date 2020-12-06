{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Exercise where

import Options.Generic

data Exercise
    = ES1 FilePath
    | ES2 FilePath
    | ES3 FilePath
    | ES4 FilePath
    | ES5 FilePath
    | ES6 FilePath
    | A19E1 FilePath
    | A19E2 FilePath
    | A19E3 FilePath
    | None
    deriving (Generic, Show)

instance ParseRecord Exercise