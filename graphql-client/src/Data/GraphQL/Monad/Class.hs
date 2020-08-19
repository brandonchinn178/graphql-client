{-|
Module      :  Data.GraphQL.Monad.Class
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the 'MonadGraphQLQuery' type class, which defines how GraphQL queries should be run.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.GraphQL.Monad.Class
  ( MonadGraphQLQuery(..)
  , runQuery
  ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Aeson.Schema (Object)
import Data.Maybe (fromJust)

import Data.GraphQL.Error (GraphQLException(..))
import Data.GraphQL.Query (GraphQLQuery(..))
import Data.GraphQL.Result (GraphQLResult, getErrors, getResult)

-- | A type class for monads that can run GraphQL queries.
class Monad m => MonadGraphQLQuery m where
  runQuerySafe
    :: (GraphQLQuery query, schema ~ ResultSchema query)
    => query -> m (GraphQLResult (Object schema))

-- | Runs the given query and returns the result, erroring if the query returned errors.
runQuery
  :: (MonadIO m, MonadGraphQLQuery m, GraphQLQuery query, schema ~ ResultSchema query)
  => query -> m (Object schema)
runQuery query = do
  result <- runQuerySafe query
  case getErrors result of
    [] -> return $ fromJust $ getResult result
    errors -> liftIO $ throwIO $ GraphQLException errors

{- Instances for common monad transformers -}

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadGraphQLQuery m) => MonadGraphQLQuery (t m) where
  runQuerySafe = lift . runQuerySafe
