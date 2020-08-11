{-|
Module      :  Data.GraphQL.Query
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions needed by GraphQL queries.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Data.GraphQL.Query
  ( GraphQLQuery(..)
  , query
  ) where

import Data.Aeson (Value)
import Data.Aeson.Schema (SchemaType)
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (lift)

-- | A type class for defining GraphQL queries.
--
-- Should be generated via graphql-codegen-haskell. Any manual instances needs
-- to be certain that `getArgs query` satisfies the arguments defined in
-- `getQueryText query`, and that the result adheres to `ResultSchema query`.
class GraphQLQuery query where
  type ResultSchema query :: SchemaType
  getQueryName :: query -> Text
  getQueryText :: query -> Text
  getArgs :: query -> Value

-- | A quasiquoter that interpolates the given string as raw text.
--
-- Trying to avoid a dependency on raw-strings-qq
query :: QuasiQuoter
query = QuasiQuoter
  { quoteExp = liftText . Text.strip . Text.pack
  , quotePat = error "Cannot use the 'query' QuasiQuoter for patterns"
  , quoteType = error "Cannot use the 'query' QuasiQuoter for types"
  , quoteDec = error "Cannot use the 'query' QuasiQuoter for declarations"
  }
  where
    liftText s = [| Text.pack $(lift $ Text.unpack s) |]
