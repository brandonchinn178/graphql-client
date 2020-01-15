import autoBind from 'auto-bind'
import {
  GraphQLObjectType,
  GraphQLSchema,
  OperationDefinitionNode,
  print as renderGraphQLNode,
} from 'graphql'

import { PluginConfig } from './config'
import {
  ParsedEnum,
  ParsedFragments,
  ParsedOperation,
  parseSelectionSet,
  parseType,
} from './parse'
import { renderAesonSchema, renderHaskellType } from './render'

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

    const args = (node.variableDefinitions ?? []).map((variableDef) => {
      const type = parseType(variableDef.type)

      return {
        arg: variableDef.variable.name.value,
        type: renderHaskellType(type),
      }
    })

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

    const selections = parseSelectionSet(
      node.selectionSet,
      schemaRoot,
      this.fragments
    )

    this._operations.push({
      name,
      query: renderGraphQLNode(node),
      queryName: `${name}${opType}`,
      queryType: `${capitalName}${opType}`,
      queryFunction: `run${capitalName}${opType}`,
      argsType: `${capitalName}Args`,
      args,
      schemaType: `${capitalName}Schema`,
      schema: renderAesonSchema(selections),
    })
  }
}

const capitalize = (s: string) => s.replace(/^\w/, (c) => c.toUpperCase())
