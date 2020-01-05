import autoBind from 'auto-bind'
import {
  FragmentDefinitionNode,
  GraphQLSchema,
  OperationDefinitionNode,
  print as renderGraphQLNode,
} from 'graphql'

import { PluginConfig } from './config'

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

  constructor(
    readonly schema: GraphQLSchema,
    readonly fragments: FragmentDefinitionNode[],
    readonly config: PluginConfig
  ) {
    this._enums = []
    this._operations = []

    autoBind(this)
  }

  getEnums(): readonly ParsedEnum[] {
    return this._enums
  }

  getOperations(): readonly ParsedOperation[] {
    return this._operations
  }

  OperationDefinition(node: OperationDefinitionNode) {
    this._operations.push({
      name: 'getRecordings',
      query: renderGraphQLNode(node),
      queryName: 'getRecordingsQuery',
      queryType: 'GetRecordingsQuery',
      queryFunction: 'runGetRecordingsQuery',
      argsType: 'GetRecordingsArgs',
      args: [
        { arg: 'query', type: 'String' },
        { arg: 'first', type: 'Maybe Int' },
      ],
      schemaType: 'GetRecordingsSchema',
      schema: 'TODO',
    })
  }
}
