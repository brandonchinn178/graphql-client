{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.GraphQL.Test.Error where

import Control.Exception (Exception (..))
import Data.Aeson.KeyMap (fromList)
import qualified Data.Aeson.Types as Aeson
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.GraphQL.Error (GraphQLError (..), GraphQLErrorLoc (..), GraphQLException (..))

testErrorDisplay :: TestTree
testErrorDisplay =
  testGroup
    "GraphQLException"
    [ testCase "displayException" $ do
        let err =
              GraphQLException
                [ GraphQLError
                    { -- TODO: What do actual error messages look like?
                      message = "Something went wrong!"
                    , locations = Just [GraphQLErrorLoc{errorLine = 10, errorCol = 3}]
                    , path = Just [Aeson.String "my_query.graphql"]
                    , extensions = Just (Aeson.Object (fromList [("complexity", Aeson.Number 2)]))
                    }
                ]
        displayException err
          @?= "GraphQL errors:\n"
          <> "* At 10:3: Something went wrong!\n"
          <> "  Path: \"my_query.graphql\"\n"
          <> "  Extensions: {\"complexity\":2}"
    ]
