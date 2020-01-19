import {
  FieldNode,
  FragmentSpreadNode,
  GraphQLInterfaceType,
  GraphQLObjectType,
  GraphQLOutputType,
  InlineFragmentNode,
  isCompositeType,
  isEnumType,
  isLeafType,
  isListType,
  isNonNullType,
  isUnionType,
  SelectionSetNode,
  TypeNode,
} from 'graphql'

import { mergeObjects } from '../utils'
import { ParsedFragments } from './fragments'

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

export type ParsedType =
  | { list: false; name: string; nullable: boolean }
  | { list: true; inner: ParsedType; nullable: boolean }

export const parseType = (type: TypeNode): ParsedType => {
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

export type ParsedSelectionSet = {
  enums: string[]
  fragments: string[]
  selections: ParsedSelection
}

export type ParsedSelection = Record<string, ParsedSelectionType>

export type ParsedSelectionType =
  | { list: false; name: string; nullable: boolean }
  | { list: false; fields: ParsedSelection; nullable: boolean }
  | { list: true; inner: ParsedSelectionType; nullable: boolean }

// GraphQL types that can be selected further into.
type GraphQLSelectionSchema = GraphQLObjectType | GraphQLInterfaceType

export const parseSelectionSet = (
  selectionSetNode: SelectionSetNode,
  schema: GraphQLSelectionSchema,
  fragments: ParsedFragments
): ParsedSelectionSet => {
  const parser = new SelectionSetParser(fragments)

  const selections = parser.parseSelectionSetNode(selectionSetNode, schema)

  return {
    enums: parser.getEnums(),
    fragments: parser.getFragments(),
    selections,
  }
}

class SelectionSetParser {
  _enums: string[]
  _fragments: string[]

  constructor(readonly allFragments: ParsedFragments) {
    this._enums = []
    this._fragments = []
  }

  getEnums(): string[] {
    return this._enums
  }

  getFragments(): string[] {
    return this._fragments
  }

  parseSelectionSetNode(
    { selections }: SelectionSetNode,
    schema: GraphQLSelectionSchema
  ): ParsedSelection {
    return mergeObjects(
      selections.map((node) => {
        switch (node.kind) {
          case 'Field':
            return this.parseFieldNode(node, schema)
          case 'FragmentSpread':
            return this.parseFragmentSpreadNode(node, schema)
          case 'InlineFragment':
            return this.parseInlineFragmentNode(node, schema)
        }

        throw new Error(`Invalid SelectionSetNode: ${node}`)
      })
    )
  }

  parseFieldNode(
    node: FieldNode,
    schema: GraphQLSelectionSchema
  ): ParsedSelection {
    const name = (node.alias ?? node.name).value

    const field = schema.getFields()[node.name.value]
    if (!field) {
      throw new Error(
        `Cannot query field "${node.name.value}" on type "${schema.name}"`
      )
    }

    return {
      [name]: this.parseSelectionType(node, field.type, schema),
    }
  }

  parseFragmentSpreadNode(
    node: FragmentSpreadNode,
    schema: GraphQLSelectionSchema
  ): ParsedSelection {
    const fragmentName = node.name.value
    this._fragments.push(fragmentName)

    const { selectionSet } = this.allFragments[fragmentName]
    return this.parseSelectionSetNode(selectionSet, schema)
  }

  /* eslint-disable-next-line class-methods-use-this */
  parseInlineFragmentNode(
    node: InlineFragmentNode,
    schema: GraphQLSelectionSchema
  ): ParsedSelection {
    /* eslint-disable-next-line no-console */
    console.log(node, schema)
    throw new Error('TODO')
  }

  parseSelectionType(
    node: FieldNode,
    type: GraphQLOutputType,
    schema: GraphQLSelectionSchema
  ): ParsedSelectionType {
    if (isNonNullType(type)) {
      return {
        ...this.parseSelectionType(node, type.ofType, schema),
        nullable: false,
      }
    }

    if (isListType(type)) {
      return {
        list: true,
        inner: this.parseSelectionType(node, type.ofType, schema),
        nullable: true,
      }
    }

    if (isLeafType(type)) {
      if (isEnumType(type)) {
        this._enums.push(type.name)
      }
      return {
        list: false,
        name: type.name,
        nullable: true,
      }
    }

    if (isCompositeType(type)) {
      if (!node.selectionSet) {
        throw new Error(
          `Field "${node.name}" of type "${schema.name}" must have a selection of subfields. Did you mean "${node.name} { ... }"?`
        )
      }

      if (isUnionType(type)) {
        throw new Error('TODO: union type')
      }

      return {
        list: false,
        fields: this.parseSelectionSetNode(node.selectionSet, type),
        nullable: true,
      }
    }

    throw new Error(`Unknown GraphQLOutputType: ${type}`)
  }
}
