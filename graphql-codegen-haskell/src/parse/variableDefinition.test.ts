import { OperationDefinitionNode } from 'graphql'
import gql from 'graphql-tag'

import { parseVariableDefinitions } from './variableDefinition'

it('parses variable definitions', () => {
  const query = gql`
    query(
      $int: Int
      $float: Float
      $string: String
      $bool: Boolean
      $id: ID
      $scalar: MyScalar
      $nonNull: String!
      $list: [Int]
      $listList: [[Int]]
      $nonNullList: [Int]!
      $nonNullListNonNull: [Int!]!
    ) {
      id
    }
  `

  const [definition] = query.definitions as OperationDefinitionNode[]

  expect(
    parseVariableDefinitions(definition.variableDefinitions ?? [])
  ).toStrictEqual({
    args: [
      {
        name: 'int',
        type: {
          list: false,
          name: 'Int',
          nullable: true,
        },
      },
      {
        name: 'float',
        type: {
          list: false,
          name: 'Float',
          nullable: true,
        },
      },
      {
        name: 'string',
        type: {
          list: false,
          name: 'String',
          nullable: true,
        },
      },
      {
        name: 'bool',
        type: {
          list: false,
          name: 'Boolean',
          nullable: true,
        },
      },
      {
        name: 'id',
        type: {
          list: false,
          name: 'ID',
          nullable: true,
        },
      },
      {
        name: 'scalar',
        type: {
          list: false,
          name: 'MyScalar',
          nullable: true,
        },
      },
      {
        name: 'nonNull',
        type: {
          list: false,
          name: 'String',
          nullable: false,
        },
      },
      {
        name: 'list',
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
      {
        name: 'listList',
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
      {
        name: 'nonNullList',
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
      {
        name: 'nonNullListNonNull',
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
    ],
  })
})
