{-|
Module      :  Data.GraphQL.Query
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions needed by GraphQL queries.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Data.GraphQL.Query
  ( Query(UnsafeQuery)
  , GraphQLArgs(..)
  , fromQuery
  , queryName
  -- * Instantiating a query
  , query
  ) where

import Data.Aeson (Value)
import Data.Aeson.Schema (SchemaType)
import Data.Kind (Type)
import Data.Text (Text)
import Language.Haskell.TH.Quote (QuasiQuoter)
import qualified Text.RawString.QQ as RawString

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
query = RawString.r
