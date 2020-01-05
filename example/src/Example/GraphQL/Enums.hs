{-# LANGUAGE TemplateHaskell #-}

module Example.GraphQL.Enums where

import Data.Aeson.Schema.TH (mkEnum)

mkEnum "ReleaseStatus"
  [ "OFFICIAL"
  , "PROMOTION"
  , "BOOTLEG"
  , "PSEUDORELEASE"
  ]
