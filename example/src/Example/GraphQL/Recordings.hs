{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Example.GraphQL.Recordings where

import Data.Aeson (object, (.=))
import Data.GraphQL hiding (Query, query)
import qualified Data.GraphQL as GraphQL

import Example.GraphQL.Enums (ReleaseStatus)
import Example.GraphQL.Scalars.Date (Date)
import Example.GraphQL.Scalars.Duration (Duration)

type Query = GraphQL.Query Args Schema

data Args = Args
  { _query :: String
  , _first :: Maybe Int
  } deriving (Show)

instance GraphQLArgs Args where
  fromArgs args = object
    [ "query" .= _query args
    , "first" .= _first args
    ]

query :: Query
query = [GraphQL.query|
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

type Schema = [schema|
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
