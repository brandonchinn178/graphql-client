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
  graphqlUnion,
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
          ...fooInfo
        }
      }

      fragment fooInfo on Foo {
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

it('parses fragment spreads for interfaces', () => {
  const schema = buildASTSchema(
    gql`
      interface Named {
        name: String!
      }

      type Dog implements Named {
        name: String!
        color: String
      }

      type Human implements Named {
        name: String!
        age: Int!
      }

      type Cat implements Named {
        name: String!
        speed: Int
      }

      type Query {
        named: Named!
      }
    `
  )

  const selectionSet = parseSelectionSetAST(
    schema,
    gql`
      query {
        named {
          name
          ...human
          ...fullDog
        }
      }

      fragment human on Human {
        age
      }

      fragment fullDog on Dog {
        name
        color
      }
    `
  )

  expect(selectionSet).toMatchObject({
    selections: {
      named: graphqlObject({
        name: graphqlScalar('String'),
        __fragments: graphqlUnion([
          {
            age: graphqlScalar('Int'),
          },
          {
            name: graphqlScalar('String'),
            color: graphqlScalar('String', NULLABLE),
          },
        ]),
      }),
    },
  })
})

it('allows __fragments field', () => {
  const schema = buildASTSchema(
    gql`
      type Foo {
        __fragments: Int!
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
          __fragments
        }
      }
    `
  )

  expect(selectionSet).toMatchObject({
    selections: {
      foo: graphqlObject({
        __fragments: graphqlScalar('Int'),
      }),
    },
  })
})

it('allows __fragments field when using a non-abstract fragment', () => {
  const schema = buildASTSchema(
    gql`
      type Foo {
        __fragments: Int!
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
        __fragments
      }
    `
  )

  expect(selectionSet).toMatchObject({
    selections: {
      foo: graphqlObject({
        __fragments: graphqlScalar('Int'),
      }),
    },
  })
})

it('disallows __fragments field when using an abstract fragment', () => {
  const schema = buildASTSchema(
    gql`
      interface Foo {
        __fragments: [String]
      }

      type Bar implements Foo {
        __fragments: [String]
        x: Int!
      }

      type Query {
        foo: Foo!
      }
    `
  )

  expect(() => {
    parseSelectionSetAST(
      schema,
      gql`
        query {
          foo {
            __fragments
            ...bar
          }
        }

        fragment bar on Bar {
          x
        }
      `
    )
  }).toThrow()
})

it('parses inline fragments', () => {
  const schema = buildASTSchema(
    gql`
      interface FooLike {
        foo: Int!
      }

      type Foo implements FooLike {
        id: ID!
        foo: Int!
      }

      type Foo2 implements FooLike {
        foo: Int!
        foo2: String!
      }

      type Query {
        foo: FooLike!
      }
    `
  )

  const selectionSet = parseSelectionSetAST(
    schema,
    gql`
      query {
        foo {
          ... on Foo {
            id
          }
          ... on Foo2 {
            foo2
          }
        }
      }
    `
  )

  expect(selectionSet).toMatchObject({
    selections: {
      foo: graphqlObject({
        __fragments: graphqlUnion([
          { id: graphqlScalar('ID') },
          { foo2: graphqlScalar('String') },
        ]),
      }),
    },
  })
})

it('parses inline fragments without type', () => {
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
          ... {
            id
          }
        }
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

it('parses inline fragments for interface without type', () => {
  const schema = buildASTSchema(
    gql`
      interface Named {
        name: String!
      }

      type Foo implements Named {
        id: ID!
        name: String!
      }

      type Query {
        named: Named!
      }
    `
  )

  const selectionSet = parseSelectionSetAST(
    schema,
    gql`
      query {
        named {
          ... {
            name
          }
        }
      }
    `
  )

  expect(selectionSet).toMatchObject({
    selections: {
      named: graphqlObject({
        name: graphqlScalar('String'),
      }),
    },
  })
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
