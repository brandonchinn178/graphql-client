import { buildASTSchema } from 'graphql'
import gql from 'graphql-tag'

import { parseFragments } from './fragments'
import { parseOperations } from './operation'

/** Tests **/

it('parses a query operation', () => {
  const query = `
    query getFoo($arg1: Int!) {
      field1
    }
  `

  expect(parseOperations(gql(query), TEST_SCHEMA, {})).toEqual({
    enums: [],
    operations: [
      {
        name: 'getFoo',

        queryText: query.trim().replace(/\n\s{4}/g, '\n'),
        queryName: 'GetFooQuery',

        args: [
          {
            name: 'arg1',
            type: {
              list: false,
              name: 'Int',
              nullable: false,
            },
          },
        ],

        schemaType: 'GetFooSchema',
        schema: {
          field1: {
            list: false,
            name: 'QueryScalar',
            nullable: false,
          },
        },
      },
    ],
  })
})

it('parses a mutation operation', () => {
  const query = `
    mutation doFoo($arg1: Int!) {
      field1
    }
  `

  expect(parseOperations(gql(query), TEST_SCHEMA, {})).toEqual({
    enums: [],
    operations: [
      {
        name: 'doFoo',

        queryText: query.trim().replace(/\n\s{4}/g, '\n'),
        queryName: 'DoFooMutation',

        args: [
          {
            name: 'arg1',
            type: {
              list: false,
              name: 'Int',
              nullable: false,
            },
          },
        ],

        schemaType: 'DoFooSchema',
        schema: {
          field1: {
            list: false,
            name: 'MutationScalar',
            nullable: false,
          },
        },
      },
    ],
  })
})

it('parses a subscription operation', () => {
  const query = `
    subscription getFoo($arg1: Int!) {
      field1
    }
  `

  expect(parseOperations(gql(query), TEST_SCHEMA, {})).toEqual({
    enums: [],
    operations: [
      {
        name: 'getFoo',

        queryText: query.trim().replace(/\n\s{4}/g, '\n'),
        queryName: 'GetFooSubscription',

        args: [
          {
            name: 'arg1',
            type: {
              list: false,
              name: 'Int',
              nullable: false,
            },
          },
        ],

        schemaType: 'GetFooSchema',
        schema: {
          field1: {
            list: false,
            name: 'SubscriptionScalar',
            nullable: false,
          },
        },
      },
    ],
  })
})

it('errors if no root schema', () => {
  const ast = gql`
    mutation doFoo($arg1: Int!) {
      field1
    }
  `

  const schema = buildASTSchema(
    gql`
      type Query {
        field1: Int
      }
    `
  )

  expect(() => parseOperations(ast, schema, {})).toThrow()
})

it('no args', () => {
  const ast = gql`
    query getFoo {
      field1
    }
  `

  // mockParseVariableDefinitions.mockReturnValue([])

  expect(parseOperations(ast, TEST_SCHEMA, {})).toMatchObject({
    operations: [
      expect.objectContaining({
        args: [],
      }),
    ],
  })
})

it('generates unique names for unnamed queries', () => {
  const ast = gql`
    query {
      field1
    }
    query getFoo {
      field1
    }
    mutation {
      field1
    }
  `

  expect(parseOperations(ast, TEST_SCHEMA, {})).toMatchObject({
    operations: [
      expect.objectContaining({
        name: 'unnamed0',
        queryName: 'Unnamed0Query',
        schemaType: 'Unnamed0Schema',
      }),
      expect.objectContaining({
        name: 'getFoo',
      }),
      expect.objectContaining({
        name: 'unnamed1',
        queryName: 'Unnamed1Mutation',
        schemaType: 'Unnamed1Schema',
      }),
    ],
  })
})

it('collects enums', () => {
  const ast = gql`
    query getMyEnum {
      myEnum
    }
  `

  expect(parseOperations(ast, TEST_SCHEMA, {})).toMatchObject({
    enums: [{ name: 'MyEnum', values: ['FOO', 'BAR'] }],
  })
})

it('renders fragments in query document', () => {
  const query = `
    query getFoo {
      ...field1Fragment
    }
    fragment field1Fragment on Query {
      field1
    }
  `
  const ast = gql(query)
  const fragments = parseFragments(ast)

  const { operations } = parseOperations(ast, TEST_SCHEMA, fragments)

  expect(operations).toHaveLength(1)
  expect(operations[0].queryText).toMatchInlineSnapshot(`
    "query getFoo {
      ...field1Fragment
    }
    fragment field1Fragment on Query {
      field1
    }"
  `)
})

/** Constants **/

const TEST_SCHEMA = buildASTSchema(
  gql`
    type Query {
      field1: QueryScalar!
      myEnum: MyEnum!
    }

    type Mutation {
      field1: MutationScalar!
    }

    type Subscription {
      field1: SubscriptionScalar!
    }

    scalar QueryScalar
    scalar MutationScalar
    scalar SubscriptionScalar

    enum MyEnum {
      FOO
      BAR
    }
  `
)
