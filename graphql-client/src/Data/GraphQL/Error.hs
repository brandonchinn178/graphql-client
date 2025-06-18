{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      :  Data.GraphQL.Error
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Definitions for GraphQL errors and exceptions.
-}
module Data.GraphQL.Error (
  GraphQLError (..),
  GraphQLErrorLoc (..),
  GraphQLException (..),
) where

import Control.Exception (Exception (..))
import Data.Aeson (FromJSON (..), ToJSON, Value, withObject, (.:))
import Data.Aeson.Text (encodeToLazyText)
import Data.List (intercalate, intersperse)
import Data.Text (Text, unpack)
import Data.Text.Lazy (toStrict)
import GHC.Generics (Generic)

-- | An error in a GraphQL query.
data GraphQLError = GraphQLError
  { message :: Text
  , locations :: Maybe [GraphQLErrorLoc]
  , path :: Maybe [Value]
  , extensions :: Maybe Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

displayGraphQLError :: String -> String -> GraphQLError -> String
displayGraphQLError indentFirst indentRest exception =
  indentFirst
    <> "At "
    <> joinWithCommas (maybe [] (map displayErrorLoc) (locations exception))
    <> ": "
    -- Note: If this message contains multiple lines, subsequent lines will not be indented correctly.
    <> unpack (message exception)
    <> maybe "" (nextLine "Path: " . joinWithCommas . map valueToString) (path exception)
    <> maybe "" (nextLine "Extensions: " . valueToString) (extensions exception)
  where
    joinWithCommas = unwords . intersperse ", "

    valueToString :: Value -> String
    valueToString = unpack . toStrict . encodeToLazyText

    nextLine prefix line = "\n" <> indentRest <> prefix <> line

-- | A location in an error in a GraphQL query.
data GraphQLErrorLoc = GraphQLErrorLoc
  { errorLine :: Int
  , errorCol :: Int
  }
  deriving (Show, Eq, Generic, ToJSON)

displayErrorLoc :: GraphQLErrorLoc -> String
displayErrorLoc errorLoc =
  show (errorLine errorLoc) <> ":" <> show (errorCol errorLoc)

instance FromJSON GraphQLErrorLoc where
  parseJSON = withObject "GraphQLErrorLoc" $ \o ->
    GraphQLErrorLoc
      <$> o .: "line"
      <*> o .: "column"

-- | An exception thrown as a result of an error in a GraphQL query.
newtype GraphQLException = GraphQLException [GraphQLError]
  deriving (Show, Eq)

instance Exception GraphQLException where
  displayException (GraphQLException errors) =
    "GraphQL errors:\n"
      -- NOTE: `unlines` appends an extra `\n` at the end, which we don't want.
      <> intercalate "\n" (map (displayGraphQLError "* " "  ") errors)
