{-|
Module      :  Data.GraphQL.TestUtils
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines test utilities for testing GraphQL queries.
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.GraphQL.TestUtils
  ( ResultMock(..)
  , mocked
  , MockQueryT
  , runMockQueryT
  , AnyResultMock
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT, state)
import Control.Monad.Trans.Class (MonadTrans)
import Data.Aeson (FromJSON, Value, object, (.=))
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text

import Data.GraphQL.Error (GraphQLError)
import Data.GraphQL.Monad (MonadGraphQLQuery(..))
import Data.GraphQL.Query (GraphQLQuery(..))

data ResultMock query = ResultMock
  { query  :: query
  , result :: Value
  } deriving (Show)

mocked :: (Show query, GraphQLQuery query) => ResultMock query -> AnyResultMock
mocked = AnyResultMock

{- AnyResultMock -}

data AnyResultMock = forall query. (Show query, GraphQLQuery query) => AnyResultMock (ResultMock query)

deriving instance Show AnyResultMock

isMatch :: GraphQLQuery query => query -> AnyResultMock -> Bool
isMatch testQuery (AnyResultMock mock) = getArgs (query mock) == getArgs testQuery

getResult :: AnyResultMock -> Value
getResult (AnyResultMock mock) = result mock

{- MockQueryT -}

newtype MockQueryT m a = MockQueryT { unMockQueryT :: StateT [AnyResultMock] m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState [AnyResultMock], MonadTrans)

instance Monad m => MonadGraphQLQuery (MockQueryT m) where
  runQuerySafe testQuery = toGraphQLResult <$> lookupMock
    where
      takeWhere :: (a -> Bool) -> [a] -> Maybe (a, [a])
      takeWhere f xs = case break f xs of
        (before, match:after) -> Just (match, before ++ after)
        (_, []) -> Nothing

      -- Find the first matching mock and remove it from the state
      lookupMock :: MockQueryT m Value
      lookupMock = state $ \mocks ->
        case takeWhere (isMatch testQuery) mocks of
          Just (mock, mocks') -> (getResult mock, mocks')
          Nothing -> error $ "No more mocked responses for query: " ++ Text.unpack (getQueryName testQuery)

      toGraphQLResult :: FromJSON a => Value -> a
      toGraphQLResult mockData = either error id . Aeson.parseEither Aeson.parseJSON $ object
        [ "errors" .= ([] :: [GraphQLError])
        , "data"   .= Just mockData
        ]

runMockQueryT :: Monad m => MockQueryT m a -> [AnyResultMock] -> m a
runMockQueryT mockQueryT mocks = (`evalStateT` mocks) . unMockQueryT $ mockQueryT
