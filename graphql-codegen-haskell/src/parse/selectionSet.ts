import {
  assertCompositeType,
  FieldNode,
  FragmentSpreadNode,
  GraphQLInterfaceType,
  GraphQLObjectType,
  GraphQLOutputType,
  InlineFragmentNode,
  isEnumType,
  isLeafType,
  isListType,
  isNonNullType,
  isUnionType,
  SelectionNode,
  SelectionSetNode,
} from 'graphql'

import { mergeObjects } from '../utils'
import { ParsedFragments } from './fragments'
import {
  ParsedListType,
  ParsedObjectType,
  ParsedScalarType,
} from './graphqlTypes'

export type ParsedSelectionSet = {
  enums: string[]
  fragments: string[]
  selections: ParsedSelection
}

export type ParsedSelection = Record<string, ParsedSelectionType>

export type ParsedSelectionType =
  | ParsedScalarType
  | ParsedListType<ParsedSelectionType>
  | ParsedObjectType<ParsedSelection>

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
      selections.map((node: SelectionNode) => {
        switch (node.kind) {
          case 'Field':
            return this.parseFieldNode(node, schema)
          case 'FragmentSpread':
            return this.parseFragmentSpreadNode(node, schema)
          case 'InlineFragment':
            return this.parseInlineFragmentNode(node, schema)
        }
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

    assertCompositeType(type)

    if (!node.selectionSet) {
      throw new Error(
        `Field "${node.name.value}" of type "${schema.name}" must have a selection of subfields. Did you mean "${node.name.value} { ... }"?`
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
}
