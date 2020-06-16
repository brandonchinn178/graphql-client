# graphql-client

![CircleCI](https://img.shields.io/circleci/build/github/LeapYear/graphql-client)
![Hackage](https://img.shields.io/hackage/v/graphql-client)
[![codecov](https://codecov.io/gh/LeapYear/graphql-client/branch/master/graph/badge.svg?token=WIOxotqPTN)](https://codecov.io/gh/LeapYear/graphql-client)

A client for Haskell applications to query GraphQL APIs. This project comes in
two parts:

* `graphql-client`: The Haskell package that can query GraphQL APIs
* `graphql-codegen-haskell`: A NodeJS library that implements a
  [`graphql-code-generator`](https://graphql-code-generator.com/) plugin to
  generate a Haskell module containing Haskell types and functions that work
  with the `graphql-client` library

## Quickstart

1. Write the `.graphql` queries you wish to use.

1. Install `graphql-code-generator` and run `graphql-codegen` after writing an
   appropriate `codegen.yml` configuration. It should look something like:

    ```yaml
    schema: https://example.com/graphql
    documents: path/to/files/*.graphql
    generates:
      src/Example/GraphQL/API.hs:
        config:
          # The Haskell module that will contain generated modules for GraphQL enums
          enumsModule: Example.GraphQL.Enums

          # The Haskell module containing the data types to use for GraphQL Scalars
          scalarsModule: Example.GraphQL.Scalars
        plugins:
          - graphql-codegen-haskell
    ```

    See `example/` for more details.

1. Run `graphql-codegen`

1. The Haskell file specified (e.g. `src/Example/GraphQL/API.hs`) should have
   been generated with the Haskell types and functions needed to run your
   `.graphql` queries.

The generated API created a function for each GraphQL query of the form
`run{queryName}Query` (or `run{queryName}Mutation` for mutations). For example,
for a query named `getRecording`:

```haskell
runGetRecordingsQuery :: MonadQuery m => GetRecordingsArgs -> m (Object GetRecordingsSchema)
```

An example usage of the API:

```haskell
import Control.Monad.IO.Class (MonadIO(..))
import Data.GraphQL (defaultQuerySettings, get, runQueryT)
import qualified Data.Text as Text

import Example.GraphQL.API

app :: (MonadQuery m, MonadIO m) => m ()
app = do
  song <- Text.pack <$> getLine

  result <- runGetRecordingsQuery GetRecordingsArgs
    { _query = song
    , _first = Just 5
    }

  -- See the `aeson-schemas` package for more information on this syntax
  let songs = [get| result.search!.recordings!.nodes![]! |]
  liftIO $ print $ map [get| .title! |] songs

main :: IO ()
main = do
  let querySettings = defaultQuerySettings
        { url = "https://graphbrainz.herokuapp.com/"
        }

  runQueryT querySettings app
```

## Testing

This library also provides utilities to test functions using GraphQL queries by
mocking the GraphQL endpoints. For example, you might test the `app` function
from the Quickstart with the following:

```haskell
import Data.Aeson.QQ (aesonQQ)
import Data.GraphQL.TestUtils (ResultMock(..), mocked, runMockQueryT)

import Example (app)
import Example.GraphQL.API

main :: IO ()
main = do
  let mockedGetRecordings = mocked ResultMock
        { query = getRecordingsQuery
        , args = GetRecordingsArgs
            { _query = "My Song"
            , _first = Just 5
            }
        , result =
            [aesonQQ|
              {
                "search": {
                  "recordings": {
                    "nodes": []
                  }
                }
              }
            |]
        }

  -- should not hit the server
  result <- runMockQueryT app [mockedGetRecordings]

  -- test `result`, which should be the result hardcoded above
```
