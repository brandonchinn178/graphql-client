{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Data.GraphQL.Test.TestQuery where

import Data.Aeson (object)
import Data.GraphQL

data TestQuery = TestQuery
  deriving (Show)

type TestSchema = [schema|
  {
    getUser: {
      id: Int
    }
  }
|]

instance GraphQLQuery TestQuery where
  type ResultSchema TestQuery = TestSchema
  getQueryName _ = "test"
  getQueryText _ = [query|
    query test($name: String!) {
      getUser(name: $name) {
        id
      }
    }
  |]
  getArgs _ = object []
