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
    enum EnumFoo {
      Foo1
      Foo2
    }

    enum EnumBar {
      Bar1
      Bar2
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

    interface Named2 {
      name: String!
    }

    type Bar2 implements Named2 {
      id: ID!
      name: String!
    }

    type Baz2 implements Named2 {
      id: ID!
      name: String!
    }

    type Query {
      enumFoo: EnumFoo
      enumBar: EnumBar
      bar(x: Int!): Bar
      getNamed(s: String!): Named
      getNamed2(s: String!): Named2
    }
  `
)

const documents = [
  {
    location: 'foo.graphql',
    document: gql`
      query getEnums {
        enumFoo
        enumBar
      }

      query getMoreEnums {
        enumFoo
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

      query getNamed2($s: String!) {
        getNamed2(s: $s) {
          ...bar2
        }
      }

      fragment bar2 on Bar2 {
        id
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
    {-# OPTIONS_GHC -w #-}

    module Example.GraphQL.API where

    import Data.GraphQL
    import Data.GraphQL.Bootstrap

    import Example.GraphQL.Scalars
    import Example.GraphQL.Enums.EnumBar
    import Example.GraphQL.Enums.EnumFoo

    {-----------------------------------------------------------------------------
    * getEnums

    -- result :: Object GetEnumsSchema; throws a GraphQL exception on errors
    result <- runQuery GetEnumsQuery
      {
      }

    -- result :: GraphQLResult (Object GetEnumsSchema)
    result <- runQuerySafe GetEnumsQuery
      {
      }
    -----------------------------------------------------------------------------}

    data GetEnumsQuery = GetEnumsQuery
      {
      }
      deriving (Show)

    type GetEnumsSchema = [schema|
      {
        enumFoo: Maybe EnumFoo,
        enumBar: Maybe EnumBar,
      }
    |]

    instance GraphQLQuery GetEnumsQuery where
      type ResultSchema GetEnumsQuery = GetEnumsSchema

      getQueryName _ = \\"getEnums\\"

      getQueryText _ = [query|
        query getEnums {
          enumFoo
          enumBar
        }
      |]

      getArgs query = object
        [
        ]

    {-----------------------------------------------------------------------------
    * getMoreEnums

    -- result :: Object GetMoreEnumsSchema; throws a GraphQL exception on errors
    result <- runQuery GetMoreEnumsQuery
      {
      }

    -- result :: GraphQLResult (Object GetMoreEnumsSchema)
    result <- runQuerySafe GetMoreEnumsQuery
      {
      }
    -----------------------------------------------------------------------------}

    data GetMoreEnumsQuery = GetMoreEnumsQuery
      {
      }
      deriving (Show)

    type GetMoreEnumsSchema = [schema|
      {
        enumFoo: Maybe EnumFoo,
      }
    |]

    instance GraphQLQuery GetMoreEnumsQuery where
      type ResultSchema GetMoreEnumsQuery = GetMoreEnumsSchema

      getQueryName _ = \\"getMoreEnums\\"

      getQueryText _ = [query|
        query getMoreEnums {
          enumFoo
        }
      |]

      getArgs query = object
        [
        ]

    {-----------------------------------------------------------------------------
    * getBar

    -- result :: Object GetBarSchema; throws a GraphQL exception on errors
    result <- runQuery GetBarQuery
      { _x = ...
      }

    -- result :: GraphQLResult (Object GetBarSchema)
    result <- runQuerySafe GetBarQuery
      { _x = ...
      }
    -----------------------------------------------------------------------------}

    data GetBarQuery = GetBarQuery
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

    instance GraphQLQuery GetBarQuery where
      type ResultSchema GetBarQuery = GetBarSchema

      getQueryName _ = \\"getBar\\"

      getQueryText _ = [query|
        query getBar($x: Int!) {
          bar(x: $x) {
            id
            foo
          }
        }
      |]

      getArgs query = object
        [ \\"x\\" .= _x (query :: GetBarQuery)
        ]

    {-----------------------------------------------------------------------------
    * getNamed

    -- result :: Object GetNamedSchema; throws a GraphQL exception on errors
    result <- runQuery GetNamedQuery
      { _s = ...
      }

    -- result :: GraphQLResult (Object GetNamedSchema)
    result <- runQuerySafe GetNamedQuery
      { _s = ...
      }
    -----------------------------------------------------------------------------}

    data GetNamedQuery = GetNamedQuery
      { _s :: Text
      }
      deriving (Show)

    type GetNamedSchema = [schema|
      {
        getNamed: Maybe {
          [__fragments]: (
            {
              id: Text,
              foo: Maybe Text,
            } |
            {
              id: Text,
              name: Text,
            }
          ),
        },
      }
    |]

    instance GraphQLQuery GetNamedQuery where
      type ResultSchema GetNamedQuery = GetNamedSchema

      getQueryName _ = \\"getNamed\\"

      getQueryText _ = [query|
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

      getArgs query = object
        [ \\"s\\" .= _s (query :: GetNamedQuery)
        ]

    {-----------------------------------------------------------------------------
    * getNamed2

    -- result :: Object GetNamed2Schema; throws a GraphQL exception on errors
    result <- runQuery GetNamed2Query
      { _s = ...
      }

    -- result :: GraphQLResult (Object GetNamed2Schema)
    result <- runQuerySafe GetNamed2Query
      { _s = ...
      }
    -----------------------------------------------------------------------------}

    data GetNamed2Query = GetNamed2Query
      { _s :: Text
      }
      deriving (Show)

    type GetNamed2Schema = [schema|
      {
        getNamed2: Maybe {
          [__fragment]: Try (
            {
              id: Text,
            }
          ),
        },
      }
    |]

    instance GraphQLQuery GetNamed2Query where
      type ResultSchema GetNamed2Query = GetNamed2Schema

      getQueryName _ = \\"getNamed2\\"

      getQueryText _ = [query|
        query getNamed2($s: String!) {
          getNamed2(s: $s) {
            ...bar2
          }
        }
        fragment bar2 on Bar2 {
          id
        }
      |]

      getArgs query = object
        [ \\"s\\" .= _s (query :: GetNamed2Query)
        ]

    "
  `)

  expect(mockWriteFileSync).toHaveBeenCalledTimes(2)

  const [writeEnumBar, writeEnumFoo] = mockWriteFileSync.mock.calls

  const [enumBarModuleName, enumBarModule] = writeEnumBar
  expect(enumBarModuleName).toBe('src/Example/GraphQL/Enums/EnumBar.hs')
  expect(enumBarModule).toMatchInlineSnapshot(`
    "{-# LANGUAGE TemplateHaskell #-}

    module Example.GraphQL.Enums.EnumBar where

    import Data.GraphQL.Bootstrap

    mkEnum \\"EnumBar\\"
      [ \\"Bar1\\"
      , \\"Bar2\\"
      ]
    "
  `)

  const [enumFooModuleName, enumFooModule] = writeEnumFoo
  expect(enumFooModuleName).toBe('src/Example/GraphQL/Enums/EnumFoo.hs')
  expect(enumFooModule).toMatchInlineSnapshot(`
    "{-# LANGUAGE TemplateHaskell #-}

    module Example.GraphQL.Enums.EnumFoo where

    import Data.GraphQL.Bootstrap

    mkEnum \\"EnumFoo\\"
      [ \\"Foo1\\"
      , \\"Foo2\\"
      ]
    "
  `)
})
