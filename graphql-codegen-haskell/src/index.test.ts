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
      foo: MyEnum
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
    import Example.GraphQL.Enums.MyEnum

    {-----------------------------------------------------------------------------
    * getFoo

    -- result :: Object GetFooSchema; throws a GraphQL exception on errors
    result <- runQuery GetFooQuery
      {
      }

    -- result :: GraphQLResult (Object GetFooSchema)
    result <- runQuerySafe GetFooQuery
      {
      }
    -----------------------------------------------------------------------------}

    data GetFooQuery = GetFooQuery
      {
      }
      deriving (Show)

    type GetFooSchema = [schema|
      {
        foo: Maybe MyEnum,
      }
    |]

    instance GraphQLQuery GetFooQuery where
      type ResultSchema GetFooQuery = GetFooSchema

      getQueryName _ = \\"getFoo\\"

      getQueryText _ = [query|
        query getFoo {
          foo
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

  expect(mockWriteFileSync).toHaveBeenCalledTimes(1)

  const [myEnumModuleName, myEnumModule] = mockWriteFileSync.mock.calls[0]
  expect(myEnumModuleName).toBe('src/Example/GraphQL/Enums/MyEnum.hs')
  expect(myEnumModule).toMatchInlineSnapshot(`
    "{-# LANGUAGE TemplateHaskell #-}

    module Example.GraphQL.Enums.MyEnum where

    import Data.GraphQL.Bootstrap

    mkEnum \\"MyEnum\\"
      [ \\"Hello\\"
      , \\"World\\"
      ]
    "
  `)
})
