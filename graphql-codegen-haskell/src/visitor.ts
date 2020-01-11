import autoBind from 'auto-bind'
import {
  FragmentDefinitionNode,
  GraphQLSchema,
  OperationDefinitionNode,
  print as renderGraphQLNode,
} from 'graphql'

import { PluginConfig } from './config'
import { ParsedEnum, ParsedOperation, parseType } from './parse'
import { renderHaskellType } from './render'

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
