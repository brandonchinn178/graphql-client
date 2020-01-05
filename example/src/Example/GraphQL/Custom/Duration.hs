{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Example.GraphQL.Custom.Duration where

import Data.Aeson (FromJSON)
import Text.Printf (printf)

-- | Duration in milliseconds.
newtype Duration = Duration Int
  deriving (Show,FromJSON)

-- | Duration in (minutes, seconds).
getDuration :: Duration -> (Int, Int)
getDuration (Duration ms) = (ms `div` 1000) `divMod` 60

-- | Duration formatted to be user-friendly.
showDuration :: Duration -> String
showDuration = uncurry (printf "%d:%02dm") . getDuration
