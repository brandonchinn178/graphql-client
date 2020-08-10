{-|
Module      :  Data.GraphQL.Monad.Class
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the 'MonadQuery' type class, which allows GraphQL
queries to be run and mocked.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.GraphQL.Monad.Class
  ( MonadQuery(..)
  , runQuery
  ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Aeson.Schema (IsSchemaObject, Object, SchemaType)
import Data.Maybe (fromJust)

import Data.GraphQL.Error (GraphQLException(..))
import Data.GraphQL.Query (GraphQLQuery(..))
import Data.GraphQL.Result (GraphQLResult, getErrors, getResult)

-- | A type class for monads that can run queries.
class Monad m => MonadQuery m where
  runQuerySafe
    :: forall query (schema :: SchemaType)
     . (GraphQLQuery query, schema ~ ResultSchema query, IsSchemaObject schema)
    => query -> m (GraphQLResult (Object schema))

-- | Runs the given query and returns the result, erroring if the query returned errors.
runQuery
  :: forall m query (schema :: SchemaType)
   . (MonadIO m, MonadQuery m, GraphQLQuery query, schema ~ ResultSchema query, IsSchemaObject schema)
  => query -> m (Object schema)
runQuery query = do
  result <- runQuerySafe query
  case getErrors result of
    [] -> return $ fromJust $ getResult result
    errors -> liftIO $ throwIO $ GraphQLException errors

{- Instances for common monad transformers -}

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadQuery m) => MonadQuery (t m) where
  runQuerySafe = lift . runQuerySafe
