{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

module Example where

import Control.Monad.IO.Class (MonadIO)
import Data.Bool (bool)
import Data.GraphQL
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as Text

import Example.GraphQL.Enums (ReleaseStatus(..))
import qualified Example.GraphQL.Recordings as Recordings
import Example.GraphQL.Scalars.Date (showDate)
import Example.GraphQL.Scalars.Duration (showDuration)

newtype App a = App { unApp :: QueryT IO a }
  deriving (Functor,Applicative,Monad,MonadIO,MonadQuery)

runApp :: App a -> IO a
runApp = runQueryT querySettings . unApp
  where
    querySettings = defaultQuerySettings
      { url = "https://graphbrainz.herokuapp.com/"
      }

mkGetter "Song" "getSongs" ''Recordings.Schema ".search!.recordings!.nodes![]!"

searchForSong :: (MonadIO m, MonadQuery m) => String -> m [Song]
searchForSong song = getSongs <$> runQuery Recordings.query Recordings.Args
  { _query = song
  , _first = Just 5
  }

showRecording :: Song -> String
showRecording song = Text.unpack $ Text.unlines $ map Text.unwords
  [ ["=====", title, parens $ Text.intercalate ", " artists, "====="]
  , ["Has video recording?", yesno $ fromMaybe False [get| song.video |]]
  , ["Length of song:", maybe "--" (Text.pack . showDuration) [get| song.length |]]
  , ["Rating:", maybe "--" fromRating mRating]
  , ["Releases:"]
  ] ++ map (("* " <>) . showRelease) [get| song.releases!.nodes![]! |]
  where
    title = [get| song.title! |]
    artists = [get| song.artists!.nodes![]!.name! |]
    (voteCount, mRating) = [get| song.rating!.(voteCount, value) |]
    fromRating rating = Text.unwords
      [ showT rating
      , parens $ Text.unwords ["out of", showT voteCount]
      ]
    showRelease release =
      if [get| release.status |] == Just OFFICIAL
        then Text.unwords
          [ [get| release.title! |]
          , maybe "--" (parens . Text.pack . showDate) [get| release.date |]
          ]
        else "[UNOFFICIAL]"

    parens s = "(" <> s <> ")"
    yesno = bool "No" "Yes"
    showT :: Show a => a -> Text.Text
    showT = Text.pack . show
