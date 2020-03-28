import * as fs from 'fs'
import { buildASTSchema } from 'graphql'
import gql from 'graphql-tag'

import { plugin, validate } from './index'

jest.mock('fs')
const mockWriteFileSync = fs.writeFileSync as jest.Mock

const fullConfig = {
  enumsModule: 'Example.GraphQL.Enums',
  scalarsModule: 'Example.GraphQL.Scalars',
}

const schema = buildASTSchema(
  gql`
    enum MyEnum {
      Hello
      World
    }

    interface Named {
      name: String!
    }

    type Bar implements Named {
      id: ID!
      foo: String
      name: String!
    }

    type Baz implements Named {
      id: ID!
      name: String!
    }

    type Query {
      foo: MyEnum
      bar(x: Int!): Bar
      getNamed(s: String!): Named
    }
  `
)

const documents = [
  {
    filePath: 'foo.graphql',
    content: gql`
      query getFoo {
        foo
      }

      query getBar($x: Int!) {
        bar(x: $x) {
          id
          foo
        }
      }

      query getNamed($s: String!) {
        getNamed(s: $s) {
          ...bar
          ...baz
        }
      }

      fragment bar on Bar {
        id
        foo
      }

      fragment baz on Baz {
        id
        name
      }
    `,
  },
]

it('validates', () => {
  expect(() => validate(schema, documents, fullConfig, '', [])).not.toThrow()
})

it('renders', () => {
  const apiModule = plugin(schema, documents, fullConfig, {
    outputFile: 'src/Example/GraphQL/API.hs',
  })

  expect(apiModule).toMatchInlineSnapshot(`
    "{-# LANGUAGE DataKinds #-}
    {-# LANGUAGE DuplicateRecordFields #-}
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE QuasiQuotes #-}
    {-# LANGUAGE TypeFamilies #-}
    {-# OPTIONS_GHC -fno-warn-unused-imports #-}

    module Example.GraphQL.API where

    import Control.Monad.IO.Class (MonadIO)
    import Data.Aeson (object, (.=))
    import Data.Aeson.Schema.TH (mkEnum)
    import Data.GraphQL
    import Data.Text (Text)

    import Example.GraphQL.Scalars
    import Example.GraphQL.Enums.MyEnum

    {-----------------------------------------------------------------------------
    * getFoo

    -- result :: Object GetFooSchema; throws a GraphQL exception on errors
    result <- runGetFooQuery GetFooArgs
      {
      }

    -- result :: GraphQLResult (Object GetFooSchema)
    result <- runGetFooQuerySafe GetFooArgs
      {
      }
    -----------------------------------------------------------------------------}

    type GetFooQuery = Query GetFooArgs GetFooSchema

    data GetFooArgs = GetFooArgs
      {
      }
      deriving (Show)

    type GetFooSchema = [schema|
      {
        foo: Maybe MyEnum,
      }
    |]

    instance GraphQLArgs GetFooArgs where
      fromArgs args = object
        [
        ]

    getFooQuery :: GetFooQuery
    getFooQuery = UnsafeQuery \\"getFoo\\" [query|
      query getFoo {
        foo
      }
    |]

    runGetFooQuery :: (MonadIO m, MonadQuery m)
      => GetFooArgs -> m (Object GetFooSchema)
    runGetFooQuery = runQuery getFooQuery

    runGetFooQuerySafe :: (MonadIO m, MonadQuery m)
      => GetFooArgs -> m (GraphQLResult (Object GetFooSchema))
    runGetFooQuerySafe = runQuerySafe getFooQuery

    {-----------------------------------------------------------------------------
    * getBar

    -- result :: Object GetBarSchema; throws a GraphQL exception on errors
    result <- runGetBarQuery GetBarArgs
      { _x = ...
      }

    -- result :: GraphQLResult (Object GetBarSchema)
    result <- runGetBarQuerySafe GetBarArgs
      { _x = ...
      }
    -----------------------------------------------------------------------------}

    type GetBarQuery = Query GetBarArgs GetBarSchema

    data GetBarArgs = GetBarArgs
      { _x :: Int
      }
      deriving (Show)

    type GetBarSchema = [schema|
      {
        bar: Maybe {
          id: Text,
          foo: Maybe Text,
        },
      }
    |]

    instance GraphQLArgs GetBarArgs where
      fromArgs args = object
        [ \\"x\\" .= _x (args :: GetBarArgs)
        ]

    getBarQuery :: GetBarQuery
    getBarQuery = UnsafeQuery \\"getBar\\" [query|
      query getBar($x: Int!) {
        bar(x: $x) {
          id
          foo
        }
      }
    |]

    runGetBarQuery :: (MonadIO m, MonadQuery m)
      => GetBarArgs -> m (Object GetBarSchema)
    runGetBarQuery = runQuery getBarQuery

    runGetBarQuerySafe :: (MonadIO m, MonadQuery m)
      => GetBarArgs -> m (GraphQLResult (Object GetBarSchema))
    runGetBarQuerySafe = runQuerySafe getBarQuery

    {-----------------------------------------------------------------------------
    * getNamed

    -- result :: Object GetNamedSchema; throws a GraphQL exception on errors
    result <- runGetNamedQuery GetNamedArgs
      { _s = ...
      }

    -- result :: GraphQLResult (Object GetNamedSchema)
    result <- runGetNamedQuerySafe GetNamedArgs
      { _s = ...
      }
    -----------------------------------------------------------------------------}

    type GetNamedQuery = Query GetNamedArgs GetNamedSchema

    data GetNamedArgs = GetNamedArgs
      { _s :: Text
      }
      deriving (Show)

    type GetNamedSchema = [schema|
      {
        getNamed: Maybe {
          __subTypes: {
            id: Text,
            foo: Maybe Text,
          } | {
            id: Text,
            name: Text,
          },
        },
      }
    |]

    instance GraphQLArgs GetNamedArgs where
      fromArgs args = object
        [ \\"s\\" .= _s (args :: GetNamedArgs)
        ]

    getNamedQuery :: GetNamedQuery
    getNamedQuery = UnsafeQuery \\"getNamed\\" [query|
      query getNamed($s: String!) {
        getNamed(s: $s) {
          ...bar
          ...baz
        }
      }
      fragment bar on Bar {
        id
        foo
      }
      fragment baz on Baz {
        id
        name
      }
    |]

    runGetNamedQuery :: (MonadIO m, MonadQuery m)
      => GetNamedArgs -> m (Object GetNamedSchema)
    runGetNamedQuery = runQuery getNamedQuery

    runGetNamedQuerySafe :: (MonadIO m, MonadQuery m)
      => GetNamedArgs -> m (GraphQLResult (Object GetNamedSchema))
    runGetNamedQuerySafe = runQuerySafe getNamedQuery

    "
  `)

  expect(mockWriteFileSync).toHaveBeenCalledTimes(1)

  const [myEnumModuleName, myEnumModule] = mockWriteFileSync.mock.calls[0]
  expect(myEnumModuleName).toBe('src/Example/GraphQL/Enums/MyEnum.hs')
  expect(myEnumModule).toMatchInlineSnapshot(`
    "{-# LANGUAGE TemplateHaskell #-}

    module Example.GraphQL.Enums.MyEnum where

    import Data.Aeson.Schema.TH (mkEnum)

    mkEnum \\"MyEnum\\"
      [ \\"Hello\\"
      , \\"World\\"
      ]
    "
  `)
})
