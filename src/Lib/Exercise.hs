{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Exercise where

import Options.Generic

data Exercise
    = A20E1 FilePath
    | A20E2 FilePath
    | A20E3 FilePath
    | A20E4 FilePath
    | A20E5 FilePath
    | A20E6 FilePath
    | A20E7 FilePath
    | A20E8 FilePath
    | A20E9 FilePath
    | A20E10 FilePath
    | A20E11 FilePath Int
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
    | A21E13 FilePath
    | A21E14 FilePath
    | A21E15 FilePath
    | A21E16 FilePath
    | A21E17 FilePath
    | A21E18 FilePath
    | A21E19 FilePath
    | A22E1 FilePath
    | A22E2 FilePath
    | A22E3 FilePath
    | A22E4 FilePath
    | A22E5 FilePath
    | A22E6 FilePath
    | A22E7 FilePath
    | A22E8 FilePath
    | A22E9 FilePath
    | A22E10 FilePath
    | A22E11 FilePath
    | None
    deriving (Generic, Show)

instance ParseRecord Exercise