{-|
Module      :  Data.GraphQL.Result
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions parsing responses from a GraphQL API.
-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.GraphQL.Result
  ( GraphQLResult(..)
  , getErrors
  , getResult
  ) where

import Data.Aeson (FromJSON(..), withObject, (.!=), (.:?))

import Data.GraphQL.Error (GraphQLError)

-- | A result of a GraphQL query.
data GraphQLResult r = GraphQLResult
  { resultErrors :: [GraphQLError]
  , resultResult :: Maybe r
  } deriving (Show,Functor,Foldable,Traversable)

instance FromJSON r => FromJSON (GraphQLResult r) where
  parseJSON = withObject "GraphQLResult" $ \o ->
    GraphQLResult
      <$> o .:? "errors" .!= []
      <*> o .:? "data"

-- | Get the errors in the @GraphQLResult@.
getErrors :: GraphQLResult r -> [GraphQLError]
getErrors = resultErrors

-- | Get the result of the @GraphQLResult@.
getResult :: GraphQLResult r -> Maybe r
getResult = resultResult
