import {
  buildASTSchema,
  DocumentNode,
  GraphQLSchema,
  Kind,
  OperationDefinitionNode,
} from 'graphql'
import gql from 'graphql-tag'

import { parseFragments } from './fragments'
import {
  graphqlList,
  graphqlObject,
  graphqlScalar,
  NULLABLE,
} from './graphqlTypes'
import { ParsedSelectionSet, parseSelectionSet } from './selectionSet'

it('parses a simple selection set', () => {
  const schema = buildASTSchema(
    gql`
      type Query {
        id: ID!
        int: Int
        string: String
        float: Float
        bool: Boolean
        custom: MyScalar
        enum: MyEnum
        unused: Boolean
      }

      enum MyEnum {
        Foo
        Bar
      }

      scalar MyScalar
    `
  )

  const selectionSet = parseSelectionSetAST(
    schema,
    gql`
      query {
        id
        int
        string
        float
        bool
        custom
        enum
      }
    `
  )

  expect(selectionSet).toMatchObject({
    enums: ['MyEnum'],
    selections: {
      id: graphqlScalar('ID'),
      int: graphqlScalar('Int', NULLABLE),
      string: graphqlScalar('String', NULLABLE),
      float: graphqlScalar('Float', NULLABLE),
      bool: graphqlScalar('Boolean', NULLABLE),
      custom: graphqlScalar('MyScalar', NULLABLE),
      enum: graphqlScalar('MyEnum', NULLABLE),
    },
  })
})

it('parses fields with aliases', () => {
  const schema = buildASTSchema(
    gql`
      type Query {
        foo: Int!
      }
    `
  )

  const selectionSet = parseSelectionSetAST(
    schema,
    gql`
      query {
        bar: foo
      }
    `
  )

  expect(selectionSet).toMatchObject({
    selections: {
      bar: graphqlScalar('Int'),
    },
  })
})

it('parses lists', () => {
  const schema = buildASTSchema(
    gql`
      type Query {
        list: [Int!]!
        nullList: [Int!]
        listNull: [Int]!
        nullListNull: [Int]
      }
    `
  )

  const selectionSet = parseSelectionSetAST(
    schema,
    gql`
      query {
        list
        nullList
        listNull
        nullListNull
      }
    `
  )

  expect(selectionSet).toMatchObject({
    selections: {
      list: graphqlList(graphqlScalar('Int')),
      nullList: graphqlList(graphqlScalar('Int'), NULLABLE),
      listNull: graphqlList(graphqlScalar('Int', NULLABLE)),
      nullListNull: graphqlList(graphqlScalar('Int', NULLABLE), NULLABLE),
    },
  })
})

it('parses objects', () => {
  const schema = buildASTSchema(
    gql`
      type Foo {
        id: ID!
        bar: Int
        unused: String
      }

      type Query {
        foo: Foo
      }
    `
  )

  const selectionSet = parseSelectionSetAST(
    schema,
    gql`
      query {
        foo {
          id
          bar
        }
      }
    `
  )

  expect(selectionSet).toMatchObject({
    selections: {
      foo: graphqlObject(
        {
          id: graphqlScalar('ID'),
          bar: graphqlScalar('Int', NULLABLE),
        },
        NULLABLE
      ),
    },
  })
})

it('parses fragment spreads', () => {
  const schema = buildASTSchema(
    gql`
      type Foo {
        id: ID!
      }

      type Query {
        foo: Foo!
      }
    `
  )

  const selectionSet = parseSelectionSetAST(
    schema,
    gql`
      query {
        foo {
          ...foo
        }
      }

      fragment foo on Foo {
        id
      }
    `
  )

  expect(selectionSet).toMatchObject({
    selections: {
      foo: graphqlObject({
        id: graphqlScalar('ID'),
      }),
    },
  })
})

it('parses inline fragments', () => {
  const schema = buildASTSchema(
    gql`
      interface FooLike {
        foo: Int!
      }

      type Foo implements FooLike
      type Foo2 implements FooLike

      type Query {
        foo: FooLike!
      }
    `
  )

  // TODO
  expect(() =>
    parseSelectionSetAST(
      schema,
      gql`
        query {
          foo {
            ... on Foo {
              foo1: foo
            }
            ... on Foo2 {
              foo2: foo
            }
          }
        }
      `
    )
  ).toThrow()
})

it('parses unions', () => {
  const schema = buildASTSchema(
    gql`
      type Foo {
        foo: Int
      }
      type Bar {
        bar: Int
      }

      union MyUnion = Foo | Bar

      type Query {
        union: MyUnion!
      }
    `
  )

  // TODO
  expect(() =>
    parseSelectionSetAST(
      schema,
      gql`
        query {
          union {
            ... on Foo {
              foo
            }
            ... on Bar {
              bar
            }
          }
        }
      `
    )
  ).toThrow()
})

it('errors when parsing an unknown field', () => {
  const schema = buildASTSchema(
    gql`
      type Query {
        foo: Int!
      }
    `
  )

  expect(() =>
    parseSelectionSetAST(
      schema,
      gql`
        query {
          bar
        }
      `
    )
  ).toThrow('Cannot query field "bar" on type "Query"')
})

it('errors when parsing an object without a selection', () => {
  const schema = buildASTSchema(
    gql`
      type Foo {
        id: ID!
      }

      type Query {
        foo: Foo!
      }
    `
  )

  expect(() =>
    parseSelectionSetAST(
      schema,
      gql`
        query {
          foo
        }
      `
    )
  ).toThrow(
    'Field "foo" of type "Query" must have a selection of subfields. Did you mean "foo { ... }"?'
  )
})

/** Helpers **/

const parseSelectionSetAST = (
  schema: GraphQLSchema,
  ast: DocumentNode
): ParsedSelectionSet => {
  const schemaRoot = schema.getQueryType()
  if (!schemaRoot) {
    throw new Error('No query type found')
  }

  const operations = ast.definitions.filter(
    (node) => node.kind === Kind.OPERATION_DEFINITION
  )
  if (operations.length !== 1) {
    throw new Error(
      `Expected exactly one operation in the ast, found: ${operations.length}`
    )
  }
  const { selectionSet } = operations[0] as OperationDefinitionNode

  const fragments = parseFragments(ast)

  return parseSelectionSet(schema, selectionSet, schemaRoot, fragments)
}
