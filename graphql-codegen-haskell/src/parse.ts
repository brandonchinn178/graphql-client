import {
  FieldNode,
  FragmentDefinitionNode,
  GraphQLInterfaceType,
  GraphQLObjectType,
  GraphQLOutputType,
  isCompositeType,
  isLeafType,
  isListType,
  isNonNullType,
  isUnionType,
  SelectionSetNode,
  TypeNode,
} from 'graphql'

import { fromPairs } from './utils'

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

export type ParsedFragments = Record<string, ParsedFragment>

export type ParsedFragment = {
  name: string
  selectionSet: SelectionSetNode
}

export const parseFragments = (
  nodes: FragmentDefinitionNode[]
): ParsedFragments =>
  fromPairs(
    nodes.map((node) => {
      const name = node.name.value
      const fragment = {
        name,
        selectionSet: node.selectionSet,
      }

      return [name, fragment]
    })
  )

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

export type ParsedSelection = Record<string, ParsedSelectionType>

export type ParsedSelectionType =
  | { list: false; name: string; nullable: boolean }
  | { list: false; fields: ParsedSelection; nullable: boolean }
  | { list: true; inner: ParsedSelectionType; nullable: boolean }

// GraphQL types that can be selected further into.
type GraphQLSelectionSchema = GraphQLObjectType | GraphQLInterfaceType

export const parseSelectionSet = (
  { selections }: SelectionSetNode,
  schema: GraphQLSelectionSchema,
  fragments: ParsedFragments
): ParsedSelection => {
  return fromPairs(
    selections.flatMap((node) => {
      switch (node.kind) {
        case 'Field': {
          const field = parseSelectionField(node, schema, fragments)
          return [field]
        }
        case 'FragmentSpread': {
          const { selectionSet } = fragments[node.name.value]
          return Object.entries(
            parseSelectionSet(selectionSet, schema, fragments)
          )
        }
        case 'InlineFragment':
          throw new Error('TODO: parse InlineFragment')
      }
    })
  )
}

const parseSelectionField = (
  node: FieldNode,
  schema: GraphQLSelectionSchema,
  fragments: ParsedFragments
): [string, ParsedSelectionType] => {
  const name = (node.alias ?? node.name).value

  const field = schema.getFields()[node.name.value]
  if (!field) {
    throw new Error(
      `Cannot query field "${node.name.value}" on type "${schema.name}"`
    )
  }

  const parseSelectionType = (type: GraphQLOutputType): ParsedSelectionType => {
    if (isNonNullType(type)) {
      return {
        ...parseSelectionType(type.ofType),
        nullable: false,
      }
    }
    if (isListType(type)) {
      return {
        list: true,
        inner: parseSelectionType(type.ofType),
        nullable: true,
      }
    }
    if (isLeafType(type)) {
      return {
        list: false,
        name: type.name,
        nullable: true,
      }
    }
    if (isCompositeType(type)) {
      if (!node.selectionSet) {
        throw new Error(
          `Field "${field.name}" of type "${schema.name}" must have a selection of subfields. Did you mean "${field.name} { ... }"?`
        )
      }
      if (isUnionType(type)) {
        throw new Error('TODO: union type')
      }

      return {
        list: false,
        fields: parseSelectionSet(node.selectionSet, type, fragments),
        nullable: true,
      }
    }

    throw new Error(`Unknown GraphQLOutputType: ${type}`)
  }

  const selectionType = parseSelectionType(field.type)

  return [name, selectionType]
}
