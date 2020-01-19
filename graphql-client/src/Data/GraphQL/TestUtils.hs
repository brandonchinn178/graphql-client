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

module Data.GraphQL.TestUtils
  ( ResultMock(..)
  , mocked
  , MockQueryT
  , runMockQueryT
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT, state)
import Data.Aeson (FromJSON, Value, object, (.=))
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text

import Data.GraphQL.Error (GraphQLError)
import Data.GraphQL.Monad (MonadQuery(..))
import Data.GraphQL.Query (GraphQLArgs(..), Query, fromQuery, queryName)

data ResultMock args schema = ResultMock
  { query  :: Query args schema
  , args   :: args
  , result :: Value
  } deriving (Show)

mocked :: GraphQLArgs args => ResultMock args schema -> AnyResultMock
mocked = AnyResultMock

{- AnyResultMock -}

data AnyResultMock = forall args schema. GraphQLArgs args => AnyResultMock (ResultMock args schema)

isMatch :: GraphQLArgs args => Query args schema -> args -> AnyResultMock -> Bool
isMatch testQuery testArgs (AnyResultMock mock) = matchesQuery && matchesArgs
  where
    matchesQuery = fromQuery (query mock) == fromQuery testQuery
    matchesArgs = fromArgs (args mock) == fromArgs testArgs

getResult :: AnyResultMock -> Value
getResult (AnyResultMock mock) = result mock

{- MockQueryT -}

newtype MockQueryT m a = MockQueryT { unMockQueryT :: StateT [AnyResultMock] m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState [AnyResultMock])

instance Monad m => MonadQuery (MockQueryT m) where
  runQuerySafe testQuery testArgs = toGraphQLResult <$> lookupMock
    where
      takeWhere :: (a -> Bool) -> [a] -> Maybe (a, [a])
      takeWhere f xs = case break f xs of
        (before, match:after) -> Just (match, before ++ after)
        (_, []) -> Nothing

      -- Find the first matching mock and remove it from the state
      lookupMock :: MockQueryT m Value
      lookupMock = state $ \mocks ->
        case takeWhere (isMatch testQuery testArgs) mocks of
          Just (mock, mocks') -> (getResult mock, mocks')
          Nothing -> error $ "No more mocked responses for query: " ++ Text.unpack (queryName testQuery)

      toGraphQLResult :: FromJSON a => Value -> a
      toGraphQLResult mockData = either error id . Aeson.parseEither Aeson.parseJSON $ object
        [ "errors" .= ([] :: [GraphQLError])
        , "data"   .= Just mockData
        ]

runMockQueryT :: Monad m => MockQueryT m a -> [AnyResultMock] -> m a
runMockQueryT mockQueryT mocks = (`evalStateT` mocks) . unMockQueryT $ mockQueryT
