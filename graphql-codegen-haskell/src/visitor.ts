import autoBind from 'auto-bind'
import {
  assertEnumType,
  GraphQLObjectType,
  GraphQLSchema,
  OperationDefinitionNode,
  print as renderGraphQLNode,
} from 'graphql'

import { PluginConfig } from './config'
import { ParsedFragments } from './parse/fragments'
import { parseSelectionSet } from './parse/selectionSet'
import { parseVariableDefinitions } from './parse/variableDefinition'
import { renderAesonSchema, renderHaskellType } from './render'

export type ParsedEnum = {
  // The name of the enum, e.g. "Status"
  name: string
  // The values in the enum, e.g. ["OPEN", "CLOSED"]
  values: string[]
}

export type ParsedOperation = {
  // The name of the operation, e.g. "getUser"
  name: string

  // The query document, e.g. "query getUser($id: Int!) { user(id: $id) { name } }"
  query: string
  // The name of the Haskell value of type Query, e.g. "getUserQuery"
  queryName: string
  // The name of the Haskell type, e.g. "GetUserQuery"
  queryType: string
  // The name of the Haskell function that runs the query, e.g. "runGetUserQuery"
  queryFunction: string

  // The name of the Haskell query args type, e.g. "GetUserArgs"
  argsType: string
  // The GraphQL arguments
  args: Array<{
    // The name of the argument
    arg: string
    // The Haskell type of the argument
    type: string
  }>

  // The name of the Haskell schema type, e.g. "GetUserSchema"
  schemaType: string
  // The schema DSL, e.g. "{ user: { name: String } }"
  schema: string
}

export class GraphQLHaskellVisitor {
  private _enums: ParsedEnum[]
  private _operations: ParsedOperation[]
  private _unnamedCounter: number

  constructor(
    readonly schema: GraphQLSchema,
    readonly fragments: ParsedFragments,
    readonly config: PluginConfig
  ) {
    this._enums = []
    this._operations = []
    this._unnamedCounter = 0

    autoBind(this)
  }

  getEnums(): readonly ParsedEnum[] {
    return this._enums
  }

  getOperations(): readonly ParsedOperation[] {
    return this._operations
  }

  OperationDefinition(node: OperationDefinitionNode) {
    const name = node.name?.value ?? `unnamed${this._unnamedCounter++}`
    const capitalName = capitalize(name)
    const opType = capitalize(node.operation)

    const args = parseVariableDefinitions(node.variableDefinitions ?? [])

    let schemaRoot: GraphQLObjectType | undefined | null
    switch (node.operation) {
      case 'query':
        schemaRoot = this.schema.getQueryType()
        break
      case 'mutation':
        schemaRoot = this.schema.getMutationType()
        break
      case 'subscription':
        schemaRoot = this.schema.getSubscriptionType()
        break
    }
    if (!schemaRoot) {
      throw new Error(
        `Unable to find root schema type for operation type "${node.operation}"`
      )
    }

    const { enums, fragments, selections } = parseSelectionSet(
      node.selectionSet,
      schemaRoot,
      this.fragments
    )

    const parsedEnums = enums.map((enumName) => {
      const enumType = assertEnumType(this.schema.getType(enumName))
      return {
        name: enumName,
        values: enumType.getValues().map(({ name }) => name),
      }
    })

    this._enums.push(...parsedEnums)

    this._operations.push({
      name,
      query: [
        renderGraphQLNode(node),
        ...fragments.map((fragment) =>
          renderGraphQLNode(this.fragments[fragment])
        ),
      ].join('\n'),
      queryName: `${name}${opType}`,
      queryType: `${capitalName}${opType}`,
      queryFunction: `run${capitalName}${opType}`,
      argsType: `${capitalName}Args`,
      args: args.map((arg) => ({
        ...arg,
        type: renderHaskellType(arg.type),
      })),
      schemaType: `${capitalName}Schema`,
      schema: renderAesonSchema(selections),
    })
  }
}

const capitalize = (s: string) => s.replace(/^\w/, (c) => c.toUpperCase())
