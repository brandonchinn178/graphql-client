{- |
Module      :  Data.GraphQL
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Core functionality for querying GraphQL APIs.
-}
module Data.GraphQL (module X) where

import Data.Aeson.Schema as X

import Data.GraphQL.Monad as X
import Data.GraphQL.Query as X
import Data.GraphQL.Result as X
