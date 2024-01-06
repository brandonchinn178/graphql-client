module Example.Scalars where

import Control.Monad (guard)
import Data.Aeson (FromJSON (..), ToJSON (..))

newtype Age = Age Int
  deriving (Show)

instance FromJSON Age where
  parseJSON v = do
    n <- parseJSON v
    guard $ n >= 0
    pure $ Age n

instance ToJSON Age where
  toJSON (Age n) = toJSON n
