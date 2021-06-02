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

import Example.GraphQL.API (GetRecordingsQuery (..), GetRecordingsSchema)
import Example.GraphQL.Enums.ReleaseStatus (ReleaseStatus (..))
import Example.GraphQL.Scalars.Date (showDate)
import Example.GraphQL.Scalars.Duration (showDuration)

newtype App a = App {unApp :: GraphQLQueryT IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadGraphQLQuery)

runApp :: App a -> IO a
runApp = runGraphQLQueryT graphQLSettings . unApp
  where
    graphQLSettings =
      defaultGraphQLSettings
        { url = "https://graphbrainz.herokuapp.com/"
        }

mkGetter "Song" "getSongs" ''GetRecordingsSchema ".search!.recordings!.nodes![]!"

searchForSong :: (MonadIO m, MonadGraphQLQuery m) => String -> m [Song]
searchForSong song =
  getSongs
    <$> runQuery
      GetRecordingsQuery
        { _query = Text.pack song
        , _first = Just 5
        }

showRecording :: Song -> String
showRecording song =
  Text.unpack $
    Text.unlines $
      map
        Text.unwords
        [ ["=====", title, parens $ Text.intercalate ", " artists, "====="]
        , ["Has video recording?", yesno $ Just True == [get| song.video |]]
        , ["Length of song:", maybe "--" (Text.pack . showDuration) [get| song.length |]]
        , ["Rating:", maybe "--" fromRating mRating]
        , ["Releases:"]
        ]
        ++ map (("* " <>) . showRelease) [get| song.releases!.nodes![]! |]
  where
    title = [get| song.title! |]
    artists = [get| song.artists!.nodes![]!.name! |]
    (voteCount, mRating) = [get| song.rating!.(voteCount, value) |]
    fromRating rating =
      Text.unwords
        [ showT rating
        , parens $ Text.unwords ["out of", showT voteCount]
        ]
    showRelease release =
      if [get| release.status |] == Just OFFICIAL
        then
          Text.unwords
            [ [get| release.title! |]
            , maybe "--" (parens . Text.pack . showDate) [get| release.date |]
            ]
        else "[UNOFFICIAL]"

    parens s = "(" <> s <> ")"
    yesno = bool "No" "Yes"
    showT :: Show a => a -> Text.Text
    showT = Text.pack . show
