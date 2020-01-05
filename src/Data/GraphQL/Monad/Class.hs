{-|
Module      :  Data.GraphQL.Monad.Class
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the 'MonadQuery' type class, which allows GraphQL
queries to be run and mocked.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Data.GraphQL.Monad.Class
  ( MonadQuery(..)
  , runQuery
  ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Data.Aeson.Schema (IsSchemaObject, Object, SchemaType)
import Data.Maybe (fromJust)

import Data.GraphQL.Error (GraphQLException(..))
import Data.GraphQL.Query (GraphQLArgs(..), Query)
import Data.GraphQL.Result (GraphQLResult, getErrors, getResult)

-- | A type class for monads that can run queries.
class Monad m => MonadQuery m where
  runQuerySafe
    :: forall args (schema :: SchemaType)
     . (GraphQLArgs args, IsSchemaObject schema)
    => Query args schema -> args -> m (GraphQLResult (Object schema))

-- | Runs the given query and returns the result, erroring if the query returned errors.
runQuery
  :: forall m args (schema :: SchemaType)
   . (MonadIO m, MonadQuery m, GraphQLArgs args, IsSchemaObject schema)
  => Query args schema -> args -> m (Object schema)
runQuery query args = do
  result <- runQuerySafe query args
  case getErrors result of
    [] -> return $ fromJust $ getResult result
    errors -> liftIO $ throwIO $ GraphQLException errors

{- Instances for common monad transformers -}

instance MonadQuery m => MonadQuery (ReaderT r m) where
  runQuerySafe query = lift . runQuerySafe query

instance MonadQuery m => MonadQuery (ExceptT e m) where
  runQuerySafe query = lift . runQuerySafe query

instance MonadQuery m => MonadQuery (IdentityT m) where
  runQuerySafe query = lift . runQuerySafe query

instance MonadQuery m => MonadQuery (MaybeT m) where
  runQuerySafe query = lift . runQuerySafe query

instance (Monoid w, MonadQuery m) => MonadQuery (Lazy.RWST r w s m) where
  runQuerySafe query = lift . runQuerySafe query

instance (Monoid w, MonadQuery m) => MonadQuery (Strict.RWST r w s m) where
  runQuerySafe query = lift . runQuerySafe query

instance MonadQuery m => MonadQuery (Lazy.StateT s m) where
  runQuerySafe query = lift . runQuerySafe query

instance MonadQuery m => MonadQuery (Strict.StateT s m) where
  runQuerySafe query = lift . runQuerySafe query

instance (Monoid w, MonadQuery m) => MonadQuery (Lazy.WriterT w m) where
  runQuerySafe query = lift . runQuerySafe query

instance (Monoid w, MonadQuery m) => MonadQuery (Strict.WriterT w m) where
  runQuerySafe query = lift . runQuerySafe query
