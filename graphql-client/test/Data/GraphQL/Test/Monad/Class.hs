{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- TypeApplications is needed in the [get| ... |] quasiquoter
{- HLint ignore "Unused LANGUAGE pragma" -}
{-# LANGUAGE TypeApplications #-}

module Data.GraphQL.Test.Monad.Class where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Schema (get)
import qualified Data.Aeson.Types as Aeson
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.GraphQL.Error (GraphQLError(..), GraphQLException(..))
import Data.GraphQL.Monad (MonadQuery(..), runQuery)
import Data.GraphQL.Result (GraphQLResult(..))
import Data.GraphQL.Test.TestQuery (TestArgs(..), testQuery)

newtype MockQueryM a = MockQueryM { unMock :: ReaderT (Either GraphQLError Value) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Either GraphQLError Value))

runMockQueryM :: Either GraphQLError Value -> MockQueryM a -> IO a
runMockQueryM mockedResult = (`runReaderT` mockedResult) . unMock

instance MonadQuery MockQueryM where
  runQuerySafe _ _ = asks toGraphQLResult
    where
      toGraphQLResult = \case
        Left e -> GraphQLResult [e] Nothing
        Right v -> GraphQLResult [] $ Aeson.parseMaybe Aeson.parseJSON v

testRunQuery :: TestTree
testRunQuery = testGroup "runQuery <-> runQuerySafe"
  [ testCase "runQuery throws if runQuerySafe returns an error" $ do
      let err = GraphQLError "Something went wrong" Nothing Nothing
      result <- try $ runMockQueryM (Left err) (runQuery testQuery TestArgs)
      show <$> result @?= Left (GraphQLException [err])

  , testCase "runQuery returns the result of runQuerySafe" $ do
      let v = object [ "getUser" .= object [ "id" .= (1 :: Int) ] ]
      result <- runMockQueryM (Right v) (runQuery testQuery TestArgs)
      [get| result.getUser.id |] @?= 1
  ]

