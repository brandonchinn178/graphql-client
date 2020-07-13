{-|
Module      :  Data.GraphQL.Bootstrap
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Imports needed for the generated API.
-}

module Data.GraphQL.Bootstrap
  ( module X
  ) where

import Control.Monad.IO.Class as X (MonadIO)
import Data.Aeson as X (object, (.=))
import Data.Aeson.Schema.TH as X (mkEnum)
import Data.Text as X (Text)
