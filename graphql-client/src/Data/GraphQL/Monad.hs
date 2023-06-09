{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      :  Data.GraphQL.Monad
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines the 'MonadGraphQLQuery' type class to query GraphQL APIs.
Also provides the 'GraphQLQueryT' monad transformer that can be
added to a transformer stack to implement the type class, and the
'runQuerySafeIO' function to manually implement it yourself.
-}
module Data.GraphQL.Monad (
  -- * MonadGraphQLQuery API
  MonadGraphQLQuery (..),
  runQuery,
  runQuerySafeIO,

  -- * GraphQLSettings
  GraphQLSettings (..),
  defaultGraphQLSettings,

  -- * GraphQLManager
  GraphQLManager,
  initGraphQLManager,

  -- * GraphQLQueryT
  GraphQLQueryT,
  runGraphQLQueryT,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (MonadTrans)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Schema (Object)
import Network.HTTP.Client (
  Manager,
  ManagerSettings,
  Request (..),
  RequestBody (..),
  httpLbs,
  newManager,
  parseUrlThrow,
  responseBody,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hContentType)

import Data.GraphQL.Monad.Class
import Data.GraphQL.Query (GraphQLQuery (..))
import Data.GraphQL.Result (GraphQLResult)

{- GraphQLSettings -}

-- | The settings for initializing a 'GraphQLManager'.
data GraphQLSettings = GraphQLSettings
  { managerSettings :: ManagerSettings
  -- ^ Uses TLS by default
  , url :: String
  , modifyReq :: Request -> Request
  }

{- | Default settings for 'GraphQLSettings'. Requires 'url' field to be overridden.

 Example usage:

 >>> defaultGraphQLSettings
 ...   { url = "https://api.github.com/graphql"
 ...   , modifyReq = \\req -> req
 ...       { requestHeaders =
 ...           (hAuthorization, "bearer my_github_token") : requestHeaders req
 ...       }
 ...   }
-}
defaultGraphQLSettings :: GraphQLSettings
defaultGraphQLSettings =
  GraphQLSettings
    { managerSettings = tlsManagerSettings
    , url = error "No URL is provided"
    , modifyReq = id
    }

{- The base runQuerySafeIO implementation -}

-- | The manager for running GraphQL queries.
data GraphQLManager = GraphQLManager
  { manager :: Manager
  , baseReq :: Request
  }

initGraphQLManager :: GraphQLSettings -> IO GraphQLManager
initGraphQLManager GraphQLSettings{..} = do
  manager <- newManager managerSettings
  baseReq <- modifyReq . modifyReq' <$> parseUrlThrow url
  return GraphQLManager{..}
  where
    modifyReq' req =
      req
        { method = "POST"
        , requestHeaders = (hContentType, "application/json") : requestHeaders req
        }

-- | Execute a GraphQL query with the given 'GraphQLManager'.
runQuerySafeIO ::
  (GraphQLQuery query, schema ~ ResultSchema query) =>
  GraphQLManager ->
  query ->
  IO (GraphQLResult (Object schema))
runQuerySafeIO GraphQLManager{..} query = httpLbs request manager >>= decodeBody
  where
    request =
      baseReq
        { requestBody =
            RequestBodyLBS $
              Aeson.encode $
                Aeson.object
                  [ "query" .= getQueryText query
                  , "variables" .= getArgs query
                  ]
        }

    decodeBody = either fail return . Aeson.eitherDecode . responseBody

{- GraphQLQueryT monad transformer -}

{- | The monad transformer type that can be used to run GraphQL queries.

 @
 newtype MyMonad a = MyMonad { unMyMonad :: GraphQLQueryT IO a }

 runMyMonad :: MyMonad a -> IO a
 runMyMonad = runGraphQLQueryT graphQLSettings . unMyMonad
   where
     graphQLSettings = defaultGraphQLSettings{url = "https://api.github.com/graphql"}
 @
-}
newtype GraphQLQueryT m a = GraphQLQueryT {unGraphQLQueryT :: ReaderT GraphQLManager m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadTrans
    )

instance MonadUnliftIO m => MonadUnliftIO (GraphQLQueryT m) where
  withRunInIO inner = GraphQLQueryT $ withRunInIO $ \run -> inner (run . unGraphQLQueryT)

instance MonadIO m => MonadGraphQLQuery (GraphQLQueryT m) where
  runQuerySafe query = do
    manager <- GraphQLQueryT ask
    liftIO $ runQuerySafeIO manager query

-- | Run the GraphQLQueryT monad transformer.
runGraphQLQueryT :: MonadIO m => GraphQLSettings -> GraphQLQueryT m a -> m a
runGraphQLQueryT settings m = do
  manager <- liftIO $ initGraphQLManager settings
  (`runReaderT` manager) . unGraphQLQueryT $ m
