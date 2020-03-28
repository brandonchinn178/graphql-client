import {
  assertCompositeType,
  assertObjectType,
  FieldNode,
  FragmentSpreadNode,
  GraphQLInterfaceType,
  GraphQLObjectType,
  GraphQLOutputType,
  GraphQLSchema,
  isAbstractType,
  isEnumType,
  isLeafType,
  isListType,
  isNonNullType,
  isUnionType,
  NamedTypeNode,
  SelectionNode,
  SelectionSetNode,
} from 'graphql'

import { mergeObjects } from '~/utils'

import { ParsedFragments } from './fragments'
import {
  graphqlList,
  graphqlObject,
  graphqlScalar,
  graphqlUnion,
  ParsedListType,
  ParsedObjectType,
  ParsedScalarType,
  ParsedUnionType,
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
  | ParsedUnionType<ParsedSelection>

// GraphQL types that can be selected further into.
type GraphQLSelectionSchema = GraphQLObjectType | GraphQLInterfaceType

export const parseSelectionSet = (
  schema: GraphQLSchema,
  selectionSetNode: SelectionSetNode,
  schemaRoot: GraphQLSelectionSchema,
  fragments: ParsedFragments
): ParsedSelectionSet => {
  const parser = new SelectionSetParser(schema, fragments)

  const selections = parser.parseSelectionSetNode(selectionSetNode, schemaRoot)

  return {
    enums: parser.getEnums(),
    fragments: parser.getFragments(),
    selections,
  }
}

class SelectionSetParser {
  _enums: string[]
  _fragments: string[]

  constructor(
    readonly schema: GraphQLSchema,
    readonly allFragments: ParsedFragments
  ) {
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
    schemaRoot: GraphQLSelectionSchema
  ): ParsedSelection {
    const parsedSubTypes: ParsedSelection[] = []
    const resolveFragmentNode = ([isSubType, selection]: [
      boolean,
      ParsedSelection
    ]) => {
      if (isSubType) {
        parsedSubTypes.push(selection)
        return {}
      }
      return selection
    }

    const selectionSet = mergeObjects(
      selections.map((node: SelectionNode) => {
        switch (node.kind) {
          case 'Field':
            return this.parseFieldNode(node, schemaRoot)
          case 'FragmentSpread':
            return resolveFragmentNode(
              this.parseFragmentSpreadNode(node, schemaRoot)
            )
          case 'InlineFragment':
            return resolveFragmentNode(this.parseFragmentNode(node, schemaRoot))
        }
      })
    )

    if (parsedSubTypes.length > 0) {
      if ('__subTypes' in selectionSet) {
        throw new Error('Please rename the "__subTypes" field in query')
      }

      return {
        ...selectionSet,
        __subTypes: graphqlUnion(parsedSubTypes),
      }
    } else {
      return selectionSet
    }
  }

  parseFieldNode(
    node: FieldNode,
    schemaRoot: GraphQLSelectionSchema
  ): ParsedSelection {
    const name = (node.alias ?? node.name).value

    const field = schemaRoot.getFields()[node.name.value]
    if (!field) {
      throw new Error(
        `Cannot query field "${node.name.value}" on type "${schemaRoot.name}"`
      )
    }

    return {
      [name]: this.parseSelectionType(node, field.type, schemaRoot),
    }
  }

  parseFragmentSpreadNode(
    node: FragmentSpreadNode,
    schemaRoot: GraphQLSelectionSchema
  ): [boolean, ParsedSelection] {
    const fragmentName = node.name.value
    this._fragments.push(fragmentName)

    return this.parseFragmentNode(this.allFragments[fragmentName], schemaRoot)
  }

  parseFragmentNode(
    fragment: { typeCondition?: NamedTypeNode; selectionSet: SelectionSetNode },
    schemaRoot: GraphQLSelectionSchema
  ): [boolean, ParsedSelection] {
    const { typeCondition, selectionSet } = fragment

    const fragmentSchema = typeCondition
      ? assertObjectType(this.schema.getType(typeCondition.name.value))
      : schemaRoot

    const selection = this.parseSelectionSetNode(selectionSet, fragmentSchema)

    const isSubType =
      isAbstractType(schemaRoot) &&
      // FIXME: use isSubType and remove type cast with graphql-js@15.0.0
      this.schema.isPossibleType(
        schemaRoot,
        fragmentSchema as GraphQLObjectType
      )
    return [isSubType, selection]
  }

  parseSelectionType(
    node: FieldNode,
    type: GraphQLOutputType,
    schemaRoot: GraphQLSelectionSchema,
    nullable = true
  ): ParsedSelectionType {
    if (isNonNullType(type)) {
      return this.parseSelectionType(node, type.ofType, schemaRoot, false)
    }

    if (isListType(type)) {
      return graphqlList(
        this.parseSelectionType(node, type.ofType, schemaRoot),
        nullable
      )
    }

    if (isLeafType(type)) {
      if (isEnumType(type)) {
        this._enums.push(type.name)
      }
      return graphqlScalar(type.name, nullable)
    }

    assertCompositeType(type)

    if (!node.selectionSet) {
      throw new Error(
        `Field "${node.name.value}" of type "${schemaRoot.name}" must have a selection of subfields. Did you mean "${node.name.value} { ... }"?`
      )
    }

    if (isUnionType(type)) {
      throw new Error('TODO: union type')
    }

    return graphqlObject(
      this.parseSelectionSetNode(node.selectionSet, type),
      nullable
    )
  }
}
