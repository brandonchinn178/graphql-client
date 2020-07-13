{- This file was automatically generated and should not be edited. -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -w #-}

module Example.GraphQL.API where

import Data.GraphQL
import Data.GraphQL.Bootstrap

import Example.GraphQL.Enums.ReleaseStatus
import Example.GraphQL.Scalars

{-----------------------------------------------------------------------------
* getRecordings

-- result :: Object GetRecordingsSchema; throws a GraphQL exception on errors
result <- runQuery GetRecordingsQuery
  { _query = ...
  , _first = ...
  }

-- result :: GraphQLResult (Object GetRecordingsSchema)
result <- runQuerySafe GetRecordingsQuery
  { _query = ...
  , _first = ...
  }
-----------------------------------------------------------------------------}

data GetRecordingsQuery = GetRecordingsQuery
  { _query :: Text
  , _first :: Maybe Int
  }
  deriving (Show)

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

instance GraphQLQuery GetRecordingsQuery where
  type ResultSchema GetRecordingsQuery = GetRecordingsSchema

  getQueryName _ = "getRecordings"

  getQueryText _ = [query|
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

  getArgs query = object
    [ "query" .= _query (query :: GetRecordingsQuery)
    , "first" .= _first (query :: GetRecordingsQuery)
    ]

