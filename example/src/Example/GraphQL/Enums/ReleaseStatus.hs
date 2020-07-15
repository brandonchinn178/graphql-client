{-# LANGUAGE TemplateHaskell #-}

module Example.GraphQL.Enums.ReleaseStatus where

import Data.GraphQL.Bootstrap

mkEnum "ReleaseStatus"
  [ "OFFICIAL"
  , "PROMOTION"
  , "BOOTLEG"
  , "PSEUDORELEASE"
  ]
