{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Aeson (ToJSON(..))
import Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.GraphQL.TestUtils (ResultMock(..), mocked, runMockQueryT)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)

import Example (searchForSong)
import Example.GraphQL.API (GetRecordingsArgs(..), getRecordingsQuery)
import Example.GraphQL.Enums.ReleaseStatus (ReleaseStatus(..))

main :: IO ()
main = defaultMain $ testGroup "searchForSong"
  [ goldens "AllStar" $ runMockQueryT (searchForSong "All Star")
      [ mockedGetRecordings "All Star"
          [ recordingAllStar
          ]
      ]
  , goldens "OldTownRoad" $ runMockQueryT (searchForSong "Old Town Road")
      [ mockedGetRecordings "Old Town Road"
          [ recordingOldTownRoad
          , recordingOldTownRoadRemix
          ]
      ]
  , goldens "NonExistentSong" $ runMockQueryT (searchForSong "Non-Existent Song")
      [ mockedGetRecordings "Non-Existent Song" []
      ]
  ]
  where
    goldens name = goldenVsString name fp . fmap (ByteString.pack . show)
      where
        fp = "test/goldens/" ++ name ++ ".golden"

    mockedGetRecordings searchQuery recordings = mocked ResultMock
      { query = getRecordingsQuery
      , args = GetRecordingsArgs
          { _query = searchQuery
          , _first = Just 5
          }
      , result =
          [aesonQQ|
            {
              "search": {
                "recordings": {
                  "nodes": #{recordings :: [Recording]}
                }
              }
            }
          |]
      }

{- Mock data -}

data Recording = Recording
  { title       :: String
  , artist      :: String
  , video       :: Bool
  , duration    :: Int
  , ratingCount :: Int
  , ratingValue :: Double
  , albumTitle  :: String
  , albumYear   :: Int
  , albumStatus :: ReleaseStatus
  } deriving (Show)

instance ToJSON Recording where
  toJSON Recording{..} =
    [aesonQQ|
      {
        "title": #{title},
        "artists": {
          "nodes": [
            {
              "name": #{artist}
            }
          ]
        },
        "video": #{video},
        "length": #{duration},
        "rating": {
          "voteCount": #{ratingCount},
          "value": #{ratingValue}
        },
        "releases": {
          "nodes": [
            {
              "title": #{albumTitle},
              "date": #{show albumYear},
              "status": #{show albumStatus}
            }
          ]
        }
      }
    |]

recordingAllStar :: Recording
recordingAllStar = Recording
  { title = "All Star"
  , artist = "Smash Mouth"
  , video = True
  , duration = 201000
  , ratingCount = 500
  , ratingValue = 4.8
  , albumTitle = "Astro Lounge"
  , albumYear = 1999
  , albumStatus = OFFICIAL
  }

recordingOldTownRoad :: Recording
recordingOldTownRoad = Recording
  { title = "Old Town Road"
  , artist = "Lil Nas X"
  , video = False
  , duration = 113000
  , ratingCount = 200
  , ratingValue = 3.4
  , albumTitle = "7"
  , albumYear = 2019
  , albumStatus = OFFICIAL
  }

recordingOldTownRoadRemix :: Recording
recordingOldTownRoadRemix = recordingOldTownRoad
  { title = "Old Town Road (Remix)"
  , artist = "Lil Nas X (feat. Billy Ray Cyrus)"
  , video = True
  , duration = 157000
  , ratingCount = 250
  , ratingValue = 4.3
  }
