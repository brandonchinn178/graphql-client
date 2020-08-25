# graphql-client

![CircleCI](https://img.shields.io/circleci/build/github/LeapYear/graphql-client)
![Hackage](https://img.shields.io/hackage/v/graphql-client)
[![codecov](https://codecov.io/gh/LeapYear/graphql-client/branch/master/graph/badge.svg?token=WIOxotqPTN)](https://codecov.io/gh/LeapYear/graphql-client)

A client for Haskell applications to query [GraphQL](https://graphql.org) APIs. This project comes in
two parts:

* `graphql-client`: The Haskell package that can query GraphQL APIs
* `graphql-codegen-haskell`: A NodeJS library that implements a
  [`graphql-code-generator`](https://graphql-code-generator.com/) plugin to
  generate a Haskell module containing Haskell types and functions that work
  with the `graphql-client` library. The generated code uses the [`aeson-schemas`](http://hackage.haskell.org/package/aeson-schemas) package to define GraphQL response types, and you'd need to use the package to extract data from the response.

## Quickstart

Pre-requisites: Have [Node.js](https://nodejs.org/) installed.

1. Add `graphql-client` as a dependency to your `package.yaml` or Cabal file

1. `stack build --only-dependencies`

1. Write the `.graphql` queries you wish to use.

1. Write an appropriate `codegen.yml` configuration. It should look something
   like:

    ```yaml
    schema: https://example.com/graphql
    documents: path/to/files/*.graphql

    hsSourceDir: src/
    apiModule: Example.GraphQL.API
    enumsModule: Example.GraphQL.Enums
    scalarsModule: Example.GraphQL.Scalars
    ```

    See `example/codegen.yml` as an example. The full specification for this file can be found in [the docs for `graphql-code-generator`](https://graphql-code-generator.com/docs/getting-started/codegen-config)

1. Write the module specified in `scalarsModule` (e.g.
   `src/Example/GraphQL/Scalars.hs`). See the "Configuration" section for more
   details.

1. `stack exec graphql-codegen`

1. The API module (e.g. `src/Example/GraphQL/API.hs`) should have been
   generated with the Haskell definitions needed to run your GraphQL queries.
   If any of your GraphQL queries use enums, corresponding modules will also
   be generated (see the "Configuration" section for more details).

The generated API creates a data type for each GraphQL query of the form
`{queryName}Query` (or `{queryName}Mutation` for mutations). For example, the following GraphQL query would generate the following Haskell code:

```graphql
query getRecordings($query: String!, $first: Int) {
  search {
    recordings(query: $query, first: $first) {
      nodes {
        title
      }
    }
  }
}
```

```haskell
data GetRecordingsQuery = GetRecordingsQuery
  { _query :: Text
  , _first :: Maybe Int
  }

type GetRecordingsSchema = [schema|
  {
    search: Maybe {
      recordings: Maybe {
        nodes: Maybe List Maybe {
          title: Maybe Text,
        },
      },
    },
  }
|]
```

`Data.GraphQL` exports a function `runQuery` which takes in one of the Query or Mutation data types and returns the response, throwing an error if the GraphQL server returns an error.

A full example of the API in action:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.IO.Class (MonadIO(..))
import Data.GraphQL
    ( MonadGraphQLQuery
    , GraphQLSettings(..)
    , defaultGraphQLSettings
    , get
    , runGraphQLQueryT
    , runQuery
    )
import qualified Data.Text as Text

import Example.GraphQL.API

app :: (MonadGraphQLQuery m, MonadIO m) => m ()
app = do
  song <- Text.pack <$> liftIO getLine

  result <- runQuery GetRecordingsQuery
    { _query = song
    , _first = Just 5
    }

  -- See the `aeson-schemas` package for more information on this syntax
  let songs = [get| result.search!.recordings!.nodes![]! |]
  liftIO $ print $ map [get| .title! |] songs

main :: IO ()
main = do
  let graphQLSettings = defaultGraphQLSettings
        { url = "https://graphbrainz.herokuapp.com/"
          -- ^ Most GraphQL APIs are at the path `/graphql`, but not this one
        }

  runGraphQLQueryT graphQLSettings app
```

## Configuration

The `codegen.yml` file should have the following format. All paths are
relative to the `codegen.yml` file.

* `schema`: Where to get the schema of the entire GraphQL API. Can be one of
  the following:

  * A URL pointing to the GraphQL API
  * The path to a local JSON file containing the result of a
    [GraphQL Introspection](https://graphql.github.io/learn/introspection/)
    query
  * The path to a local `.graphql` file containing the schema in GraphQL format

* `documents`: A string or list of strings containing
  [glob expressions](https://github.com/isaacs/node-glob) to load the
  `.graphql` files containing the GraphQL queries you wish to use.

* `hsSourceDir`: The directory (relative to `codegen.yml`) to generate the
  modules. Should be one of the directories in the `hs-source-dirs` field in
  your Cabal file. A module `X.Y.Z` would be generated at
  `<hsSourceDir>/X/Y/Z.hs`. Defaults to `src/`.

* `apiModule`: The module that will be generated with the Haskell definitions
  corresponding to the `.graphql` input files specified by `documents`.

* `enumsModule`: The module where GraphQL enums will be generated. Only the
  enums you actually use in your queries will be generated, with a module
  generated per enum. For example, if your queries use a `Color` enum and
  `enumsModule` is set to `Example.GraphQL.Enums`, `graphql-codegen` will
  generate the `Example.GraphQL.Enums.Color` module.

* `scalarsModule`: The module where custom GraphQL scalars should be exported.
  You may define the scalars in other modules, but you must re-export them in
  this module. If you're not using any custom scalars in your queries, this
  module can be empty (but must still exist). All GraphQL scalars must have a
  `FromJSON` instance.

## Testing

This library also provides utilities to test functions using GraphQL queries by
mocking the GraphQL endpoints. For example, you might test the `app` function
from the Quickstart with the following:

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Data.Aeson.QQ (aesonQQ)
import Data.GraphQL.TestUtils (ResultMock(..), mocked, runMockQueryT)

import Example (app)
import Example.GraphQL.API

main :: IO ()
main = do
  let mockedGetRecordings = mocked ResultMock
        { query = GetRecordingsQuery
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
