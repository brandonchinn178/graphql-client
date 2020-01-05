{-|
Module      :  Data.GraphQL.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the 'QueryT' monad transformer, which implements
'MonadQuery' to allow querying GraphQL APIs.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Data.GraphQL.Monad
  ( module Data.GraphQL.Monad.Class
  , QueryT
  , runQueryT
  , QuerySettings(..)
  , defaultQuerySettings
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO(..), UnliftIO(..), withUnliftIO)
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
import Data.GraphQL.Query (GraphQLArgs(..), fromQuery)

-- | The state for running QueryT.
data QueryState = QueryState
  { manager :: Manager
  , baseReq :: Request
  }

-- | The monad transformer type that should be used to run GraphQL queries.
--
-- @
-- newtype MyMonad a = MyMonad { unMyMonad :: QueryT IO a }
--
-- runMyMonad :: MyMonad a -> IO a
-- runMyMonad = runQueryT querySettings . unMyMonad
--   where
--     querySettings = defaultQuerySettings
--       { url = "https://api.github.com/graphql"
--       , modifyReq = \\req -> req
--           { requestHeaders =
--               (hAuthorization, "bearer my_github_token") : requestHeaders req
--           }
--       }
-- @
newtype QueryT m a = QueryT { unQueryT :: ReaderT QueryState m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader QueryState
    , MonadTrans
    )

instance MonadUnliftIO m => MonadUnliftIO (QueryT m) where
  askUnliftIO = QueryT $
    withUnliftIO $ \u ->
      return $ UnliftIO (unliftIO u . unQueryT)

instance MonadIO m => MonadQuery (QueryT m) where
  runQuerySafe query args = do
    QueryState{..} <- ask

    let request = baseReq
          { requestBody = RequestBodyLBS $ Aeson.encode $ Aeson.object
              [ "query" .= fromQuery query
              , "variables" .= fromArgs args
              ]
          }

    liftIO $ either fail return . Aeson.eitherDecode . responseBody =<< httpLbs request manager

-- | Run a QueryT stack.
runQueryT :: MonadIO m => QuerySettings -> QueryT m a -> m a
runQueryT QuerySettings{..} query = do
  state <- liftIO $ do
    manager <- newManager managerSettings
    baseReq <- modifyReq . modifyReq' <$> parseUrlThrow url
    return QueryState{..}

  (`runReaderT` state)
    . unQueryT
    $ query
  where
    modifyReq' req = req
      { method = "POST"
      , requestHeaders = (hContentType, "application/json") : requestHeaders req
      }

-- | The settings for running QueryT.
data QuerySettings = QuerySettings
  { managerSettings :: ManagerSettings
    -- ^ Uses TLS by default
  , url             :: String
  , modifyReq       :: Request -> Request
  }

-- | Default query settings.
defaultQuerySettings :: QuerySettings
defaultQuerySettings = QuerySettings
  { managerSettings = tlsManagerSettings
  , url = error "No URL is provided"
  , modifyReq = id
  }
