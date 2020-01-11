import autoBind from 'auto-bind'
import {
  FragmentDefinitionNode,
  GraphQLSchema,
  OperationDefinitionNode,
  print as renderGraphQLNode,
  TypeNode,
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
  private _unnamedCounter: number

  constructor(
    readonly schema: GraphQLSchema,
    readonly fragments: FragmentDefinitionNode[],
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

    const args = (node.variableDefinitions ?? []).map((variableDef) => {
      const type = parseType(variableDef.type)

      return {
        arg: variableDef.variable.name.value,
        type: renderHaskellType(type),
      }
    })

    this._operations.push({
      name,
      query: renderGraphQLNode(node),
      queryName: `${name}${opType}`,
      queryType: `${capitalName}${opType}`,
      queryFunction: `run${capitalName}${opType}`,
      argsType: `${capitalName}Args`,
      args,
      schemaType: `${capitalName}Schema`,
      schema: 'TODO',
    })
  }
}

const capitalize = (s: string) => s.replace(/^\w/, (c) => c.toUpperCase())

type ParsedType =
  | { list: false; name: string; nullable: boolean }
  | { list: true; inner: ParsedType; nullable: boolean }

const parseType = (type: TypeNode): ParsedType => {
  switch (type.kind) {
    case 'NamedType':
      return {
        list: false,
        name: type.name.value,
        nullable: true,
      }
    case 'ListType':
      return {
        list: true,
        inner: parseType(type.type),
        nullable: true,
      }
    case 'NonNullType':
      return {
        ...parseType(type.type),
        nullable: false,
      }
  }
}

const renderHaskellType = (type: ParsedType): string => {
  const baseType = type.list ? `[${renderHaskellType(type.inner)}]` : type.name
  return type.nullable ? `Maybe ${baseType}` : baseType
}
