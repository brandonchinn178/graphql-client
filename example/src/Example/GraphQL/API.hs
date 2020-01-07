{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Example.GraphQL.API where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (object, (.=))
import Data.Aeson.Schema.TH (mkEnum)
import Data.GraphQL

import Example.GraphQL.Scalars

{-----------------------------------------------------------------------------
* Enums
-----------------------------------------------------------------------------}

mkEnum "ReleaseStatus"
  [ "OFFICIAL"
  , "PROMOTION"
  , "BOOTLEG"
  , "PSEUDORELEASE"
  ]

{-----------------------------------------------------------------------------
* getRecordings

-- result :: GraphQLResult (Object GetRecordingsSchema)
result <- runGetRecordingsQuerySafe GetRecordingsArgs
  { _query = ...
  , _first = ...
  }

-- result :: Object GetRecordingsSchema; throws a GraphQL exception on errors
result <- runGetRecordingsQuery GetRecordingsArgs
  { _query = ...
  , _first = ...
  }
-----------------------------------------------------------------------------}

type GetRecordingsQuery = Query GetRecordingsArgs GetRecordingsSchema

data GetRecordingsArgs = GetRecordingsArgs
  { _query :: String
  , _first :: Maybe Int
  } deriving (Show)

type GetRecordingsSchema = [schema|
  {
    search: Maybe {
      recordings: Maybe {
        nodes: Maybe List Maybe {
          title: Maybe Text,
          artists: Maybe {
            nodes: Maybe List Maybe {
              name: Maybe Text,
            },
          },
          video: Maybe Bool,
          length: Maybe Duration,
          rating: Maybe {
            voteCount: Int,
            value: Maybe Double,
          },
          releases: Maybe {
            nodes: Maybe List Maybe {
              title: Maybe Text,
              date: Maybe Date,
              status: Maybe ReleaseStatus,
            },
          },
        },
      },
    },
  }
|]

instance GraphQLArgs GetRecordingsArgs where
  fromArgs args = object
    [ "query" .= _query args
    , "first" .= _first args
    ]

getRecordingsQuery :: GetRecordingsQuery
getRecordingsQuery = [query|
  query getRecordings($query: String!, $first: Int) {
    search {
      recordings(query: $query, first: $first) {
        nodes {
          title
          artists {
            nodes {
              name
            }
          }
          video
          length
          rating {
            voteCount
            value
          }
          releases {
            nodes {
              title
              date
              status
            }
          }
        }
      }
    }
  }
|]

runGetRecordingsQuerySafe :: (MonadIO m, MonadQuery m)
  => GetRecordingsArgs -> m (GraphQLResult (Object GetRecordingsSchema))
runGetRecordingsQuerySafe = runQuerySafe getRecordingsQuery

runGetRecordingsQuery :: (MonadIO m, MonadQuery m)
  => GetRecordingsArgs -> m (Object GetRecordingsSchema)
runGetRecordingsQuery = runQuery getRecordingsQuery
