import { buildASTSchema, DocumentNode, OperationDefinitionNode } from 'graphql'
import gql from 'graphql-tag'

import { parseVariableDefinitions } from './variableDefinition'

it('parses an Int scalar', () => {
  const { args } = parseVariableDefinitionsAST(gql`
    query($x: Int) {
      id
    }
  `)

  expect(args).toStrictEqual([
    {
      name: 'x',
      type: {
        list: false,
        name: 'Int',
        nullable: true,
      },
    },
  ])
})

it('parses a Float scalar', () => {
  const { args } = parseVariableDefinitionsAST(gql`
    query($x: Float) {
      id
    }
  `)

  expect(args).toStrictEqual([
    {
      name: 'x',
      type: {
        list: false,
        name: 'Float',
        nullable: true,
      },
    },
  ])
})

it('parses a String scalar', () => {
  const { args } = parseVariableDefinitionsAST(gql`
    query($x: String) {
      id
    }
  `)

  expect(args).toStrictEqual([
    {
      name: 'x',
      type: {
        list: false,
        name: 'String',
        nullable: true,
      },
    },
  ])
})

it('parses a Boolean scalar', () => {
  const { args } = parseVariableDefinitionsAST(gql`
    query($x: Boolean) {
      id
    }
  `)

  expect(args).toStrictEqual([
    {
      name: 'x',
      type: {
        list: false,
        name: 'Boolean',
        nullable: true,
      },
    },
  ])
})

it('parses an ID scalar', () => {
  const { args } = parseVariableDefinitionsAST(gql`
    query($x: ID) {
      id
    }
  `)

  expect(args).toStrictEqual([
    {
      name: 'x',
      type: {
        list: false,
        name: 'ID',
        nullable: true,
      },
    },
  ])
})

it('parses a custom scalar', () => {
  const { args } = parseVariableDefinitionsAST(gql`
    query($x: MyScalar) {
      id
    }
  `)

  expect(args).toStrictEqual([
    {
      name: 'x',
      type: {
        list: false,
        name: 'MyScalar',
        nullable: true,
      },
    },
  ])
})

it('parses a non null scalar', () => {
  const { args } = parseVariableDefinitionsAST(gql`
    query($x: String!) {
      id
    }
  `)

  expect(args).toStrictEqual([
    {
      name: 'x',
      type: {
        list: false,
        name: 'String',
        nullable: false,
      },
    },
  ])
})

it('parses a list', () => {
  const { args } = parseVariableDefinitionsAST(gql`
    query($x: [Int]) {
      id
    }
  `)

  expect(args).toStrictEqual([
    {
      name: 'x',
      type: {
        list: true,
        inner: {
          list: false,
          name: 'Int',
          nullable: true,
        },
        nullable: true,
      },
    },
  ])
})

it('parses a list of a list', () => {
  const { args } = parseVariableDefinitionsAST(gql`
    query($x: [[Int]]) {
      id
    }
  `)

  expect(args).toStrictEqual([
    {
      name: 'x',
      type: {
        list: true,
        inner: {
          list: true,
          inner: {
            list: false,
            name: 'Int',
            nullable: true,
          },
          nullable: true,
        },
        nullable: true,
      },
    },
  ])
})

it('parses a non null list', () => {
  const { args } = parseVariableDefinitionsAST(gql`
    query($x: [Int]!) {
      id
    }
  `)

  expect(args).toStrictEqual([
    {
      name: 'x',
      type: {
        list: true,
        inner: {
          list: false,
          name: 'Int',
          nullable: true,
        },
        nullable: false,
      },
    },
  ])
})

it('parses a non null list of a non null scalar', () => {
  const { args } = parseVariableDefinitionsAST(gql`
    query($x: [Int!]!) {
      id
    }
  `)

  expect(args).toStrictEqual([
    {
      name: 'x',
      type: {
        list: true,
        inner: {
          list: false,
          name: 'Int',
          nullable: false,
        },
        nullable: false,
      },
    },
  ])
})

const parseVariableDefinitionsAST = (query: DocumentNode) => {
  const schema = buildASTSchema(
    gql`
      type Query {
        id: ID!
      }

      scalar MyScalar
    `
  )

  const [definition] = query.definitions as OperationDefinitionNode[]
  const { variableDefinitions } = definition
  if (!variableDefinitions) {
    throw new Error('Found no variable definitions in query')
  }

  return parseVariableDefinitions(schema, variableDefinitions)
}
