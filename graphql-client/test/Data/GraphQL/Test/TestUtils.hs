{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Data.GraphQL.Test.TestUtils where

import Control.Exception (SomeException, try)
import Data.Aeson (object, (.=))
import Data.Aeson.Schema (get)
import Data.Either (isLeft)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?), (@?=))

import Data.GraphQL.Monad (runQuery)
import Data.GraphQL.Test.TestQuery (TestArgs(..), testQuery)
import Data.GraphQL.TestUtils (ResultMock(..), mocked, runMockQueryT)

testTestUtils :: TestTree
testTestUtils = testGroup "TestUtils"
  [ testCase "MockQueryT returns a mock" $ do
      obj <- runMockQueryT runTestQuery
        [ mocked ResultMock
            { query = testQuery
            , args = TestArgs
            , result = object [ "getUser" .= object [ "id" .= (20 :: Int) ] ]
            }
        ]
      [get| obj.getUser.id |] @?= 20
  , testCase "MockQueryT errors with no matching mock" $ do
      obj <- try @SomeException $ runMockQueryT runTestQuery []
      case obj of
        Right _ -> assertFailure "MockQueryT did not error"
        Left e -> (head . lines . show) e @?= "No more mocked responses for query: test"
  , testCase "MockQueryT removes mock after use" $ do
      obj <- try @SomeException $ runMockQueryT (runTestQuery >> runTestQuery)
        [ mocked ResultMock
            { query = testQuery
            , args = TestArgs
            , result = object [ "getUser" .= object [ "id" .= (20 :: Int) ] ]
            }
        ]
      isLeft obj @? "MockQueryT did not error"
  ]
  where
    runTestQuery = runQuery testQuery TestArgs
