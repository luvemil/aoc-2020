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
    | ES7 FilePath
    | ES8 FilePath
    | ES9 FilePath
    | ES10 FilePath
    | ES11 FilePath Int
    | A19E1 FilePath
    | A19E2 FilePath
    | A19E3 FilePath
    | A19E4 FilePath
    | A21E1 FilePath
    | A21E2 FilePath
    | A21E3 FilePath
    | A21E4 FilePath
    | A21E5 FilePath
    | A21E6 FilePath Int
    | A21E7 FilePath
    | A21E8 FilePath
    | A21E9 FilePath
    | A21E10 FilePath
    | A21E11 FilePath
    | A21E12 FilePath
    | None
    deriving (Generic, Show)

instance ParseRecord Exercise