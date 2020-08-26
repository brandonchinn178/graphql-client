{-|
Module      :  Data.GraphQL.Monad.Class
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the 'MonadGraphQLQuery' type class, which defines how GraphQL queries should be run.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Data.GraphQL.Monad.Class
  ( MonadGraphQLQuery(..)
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

instance MonadGraphQLQuery m => MonadGraphQLQuery (ReaderT r m) where
  runQuerySafe = lift . runQuerySafe

instance MonadGraphQLQuery m => MonadGraphQLQuery (ExceptT e m) where
  runQuerySafe = lift . runQuerySafe

instance MonadGraphQLQuery m => MonadGraphQLQuery (IdentityT m) where
  runQuerySafe = lift . runQuerySafe

instance MonadGraphQLQuery m => MonadGraphQLQuery (MaybeT m) where
  runQuerySafe = lift . runQuerySafe

instance (Monoid w, MonadGraphQLQuery m) => MonadGraphQLQuery (Lazy.RWST r w s m) where
  runQuerySafe = lift . runQuerySafe

instance (Monoid w, MonadGraphQLQuery m) => MonadGraphQLQuery (Strict.RWST r w s m) where
  runQuerySafe = lift . runQuerySafe

instance MonadGraphQLQuery m => MonadGraphQLQuery (Lazy.StateT s m) where
  runQuerySafe = lift . runQuerySafe

instance MonadGraphQLQuery m => MonadGraphQLQuery (Strict.StateT s m) where
  runQuerySafe = lift . runQuerySafe

instance (Monoid w, MonadGraphQLQuery m) => MonadGraphQLQuery (Lazy.WriterT w m) where
  runQuerySafe = lift . runQuerySafe

instance (Monoid w, MonadGraphQLQuery m) => MonadGraphQLQuery (Strict.WriterT w m) where
  runQuerySafe = lift . runQuerySafe
