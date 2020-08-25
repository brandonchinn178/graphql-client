{-|
Module      :  Data.GraphQL.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the 'GraphQLQueryT' monad transformer, which implements
'MonadGraphQLQuery' to allow querying GraphQL APIs.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Data.GraphQL.Monad
  ( module Data.GraphQL.Monad.Class
  , GraphQLQueryT
  , runGraphQLQueryT
  , GraphQLSettings(..)
  , defaultGraphQLSettings
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (MonadTrans)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Network.HTTP.Client
    ( Manager
    , ManagerSettings
    , Request(..)
    , RequestBody(..)
    , httpLbs
    , newManager
    , parseUrlThrow
    , responseBody
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hContentType)

import Data.GraphQL.Monad.Class
import Data.GraphQL.Query (GraphQLQuery(..))

-- | The state for running GraphQLQueryT.
data QueryState = QueryState
  { manager :: Manager
  , baseReq :: Request
  }

-- | The monad transformer type that should be used to run GraphQL queries.
--
-- @
-- newtype MyMonad a = MyMonad { unMyMonad :: GraphQLQueryT IO a }
--
-- runMyMonad :: MyMonad a -> IO a
-- runMyMonad = runGraphQLQueryT graphQLSettings . unMyMonad
--   where
--     graphQLSettings = defaultGraphQLSettings
--       { url = "https://api.github.com/graphql"
--       , modifyReq = \\req -> req
--           { requestHeaders =
--               (hAuthorization, "bearer my_github_token") : requestHeaders req
--           }
--       }
-- @
newtype GraphQLQueryT m a = GraphQLQueryT { unGraphQLQueryT :: ReaderT QueryState m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader QueryState
    , MonadTrans
    )

instance MonadUnliftIO m => MonadUnliftIO (GraphQLQueryT m) where
  withRunInIO inner = GraphQLQueryT $ withRunInIO $ \run -> inner (run . unGraphQLQueryT)

instance MonadIO m => MonadGraphQLQuery (GraphQLQueryT m) where
  runQuerySafe query = do
    QueryState{..} <- ask

    let request = baseReq
          { requestBody = RequestBodyLBS $ Aeson.encode $ Aeson.object
              [ "query" .= getQueryText query
              , "variables" .= getArgs query
              ]
          }

    liftIO $ either fail return . Aeson.eitherDecode . responseBody =<< httpLbs request manager

-- | Run a GraphQLQueryT stack.
runGraphQLQueryT :: MonadIO m => GraphQLSettings -> GraphQLQueryT m a -> m a
runGraphQLQueryT GraphQLSettings{..} m = do
  state <- liftIO $ do
    manager <- newManager managerSettings
    baseReq <- modifyReq . modifyReq' <$> parseUrlThrow url
    return QueryState{..}

  (`runReaderT` state)
    . unGraphQLQueryT
    $ m
  where
    modifyReq' req = req
      { method = "POST"
      , requestHeaders = (hContentType, "application/json") : requestHeaders req
      }

-- | The settings for running GraphQLQueryT.
data GraphQLSettings = GraphQLSettings
  { managerSettings :: ManagerSettings
    -- ^ Uses TLS by default
  , url             :: String
  , modifyReq       :: Request -> Request
  }

-- | Default query settings.
defaultGraphQLSettings :: GraphQLSettings
defaultGraphQLSettings = GraphQLSettings
  { managerSettings = tlsManagerSettings
  , url = error "No URL is provided"
  , modifyReq = id
  }
