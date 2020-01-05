{-|
Module      :  Data.GraphQL.Error
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for GraphQL errors and exceptions.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.GraphQL.Error
  ( GraphQLError(..)
  , GraphQLErrorLoc(..)
  , GraphQLException(..)
  ) where

import Control.Exception (Exception)
import Data.Aeson (FromJSON(..), ToJSON, Value, withObject, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | An error in a GraphQL query.
data GraphQLError = GraphQLError
  { message   :: Text
  , locations :: Maybe [GraphQLErrorLoc]
  , path      :: Maybe [Value]
  } deriving (Show,Eq,Generic,ToJSON,FromJSON)

-- | A location in an error in a GraphQL query.
data GraphQLErrorLoc = GraphQLErrorLoc
  { errorLine :: Int
  , errorCol  :: Int
  } deriving (Show,Eq,Generic,ToJSON)

instance FromJSON GraphQLErrorLoc where
  parseJSON = withObject "GraphQLErrorLoc" $ \o ->
    GraphQLErrorLoc
      <$> o .: "line"
      <*> o .: "column"

-- | An exception thrown as a result of an error in a GraphQL query.
newtype GraphQLException = GraphQLException [GraphQLError]
  deriving (Show,Exception,Eq)
