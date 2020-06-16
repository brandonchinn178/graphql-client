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

Pre-requisites: Have NodeJS installed, with `npm` or `yarn` also installed.

1. Write the `.graphql` queries you wish to use.

1. Add a `package.json` file like the following:

    ```json
    {
      "name": "my-example",
      "version": "0.0.1",
      "private": true,
      "scripts": {
        "generate": "graphql-codegen"
      },
      "devDependencies": {
        "@graphql-codegen/cli": "latest",
        "graphql": "latest",
        "graphql-codegen-haskell": "latest"
      }
    }
    ```

1. `npm install` or `yarn install`

1. Write an appropriate `codegen.yml` configuration. It should look something like:

    ```yaml
    schema: https://example.com/graphql
    documents: path/to/files/*.graphql
    generates:
      src/Example/GraphQL/API.hs:
        config:
          enumsModule: Example.GraphQL.Enums
          scalarsModule: Example.GraphQL.Scalars
        plugins:
          - graphql-codegen-haskell
    ```

    See `example/codegen.yml` as an example.

1. Write the module specified in `scalarsModule` (e.g.
   `src/Example/GraphQL/Scalars.hs`), which should export Haskell types
   corresponding to any scalars used in your queries.

   For example, if your queries use a `Date` scalar, you should implement a
   data type named `Date` with a proper `FromJSON` instance in
   `Example.GraphQL.Scalars`. You may also implement each scalar in a separate
   module, if you wish, and then re-export them in `Example.GraphQL.Scalars`.

1. `npm run generate` or `yarn generate`

1. The Haskell file specified (e.g. `src/Example/GraphQL/API.hs`) should have
   been generated with the Haskell types and functions needed to run your
   `.graphql` queries.

   If any of your GraphQL queries use enums, corresponding modules will also be
   generated in `enumsModule`. For example, if your queries use a `Color` enum,
   the module `Example.GraphQL.Enums.Color` would be automatically generated at
   `src/Example/GraphQL/Enums/Color.hs`.

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
