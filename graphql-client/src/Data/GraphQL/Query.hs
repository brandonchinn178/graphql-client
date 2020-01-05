{-|
Module      :  Data.GraphQL.Query
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions needed by GraphQL queries.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.GraphQL.Query
  ( Query
  , GraphQLArgs(..)
  , fromQuery
  , queryName
  -- * Instantiating a query
  , query
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (Value)
import Data.Aeson.Schema (SchemaType)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (lift)

-- | A type class for query arguments.
class GraphQLArgs args where
  fromArgs :: args -> Value

-- | A GraphQL Query that is validated at compile-time.
data Query (args :: Type) (schema :: SchemaType) = UnsafeQuery
  { queryName' :: Text
  , queryText  :: Text
  }
  deriving (Show)

-- | Extract the text of the Query.
fromQuery :: Query args r -> Text
fromQuery = queryText

-- | Get the name of the Query.
queryName :: Query args r -> Text
queryName = queryName'

query :: QuasiQuoter
query = QuasiQuoter
  { quoteExp = mkQuery . Text.strip . Text.pack
  , quotePat = error "Cannot use the 'query' QuasiQuoter for patterns"
  , quoteType = error "Cannot use the 'query' QuasiQuoter for types"
  , quoteDec = error "Cannot use the 'query' QuasiQuoter for declarations"
  }
  where
    mkQuery s = [| UnsafeQuery $(getName s) $(liftS s) |]
    liftS s = [| Text.pack $(lift $ Text.unpack s) |]
    getName = liftS . Text.strip . Text.takeWhile (/= '(') . dropHeader

    dropHeader s =
      fromMaybe
        (error $ "Invalid GraphQL query: " ++ Text.unpack s)
        $ ("query" `Text.stripPrefix` s) <|> ("mutation" `Text.stripPrefix` s)
