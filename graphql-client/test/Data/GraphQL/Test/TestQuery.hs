{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.GraphQL.Test.TestQuery where

import Data.Aeson (object)
import Data.GraphQL

type TestQuery = Query TestArgs TestSchema

data TestArgs = TestArgs

instance GraphQLArgs TestArgs where
  fromArgs _ = object []

testQuery :: TestQuery
testQuery = UnsafeQuery "test" [query|
  query test($name: String!) {
    getUser(name: $name) {
      id
    }
  }
|]

type TestSchema = [schema|
  {
    getUser: {
      id: Int
    }
  }
|]
