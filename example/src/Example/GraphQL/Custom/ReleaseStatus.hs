{-# LANGUAGE TemplateHaskell #-}

module Example.GraphQL.Custom.ReleaseStatus where

import Data.Aeson.Schema.TH (mkEnum)

mkEnum "ReleaseStatus"
  [ "OFFICIAL"
  , "PROMOTION"
  , "BOOTLEG"
  , "PSEUDORELEASE"
  ]
