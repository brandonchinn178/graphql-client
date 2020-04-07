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

type FragmentSelectionInfo = {
  fragment: null | {
    name: string
    isSubType: boolean
  }
  selection: ParsedSelection
}

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
    const parsedSubTypeNames: string[] = []
    const parsedSubTypes: ParsedSelection[] = []

    const resolveFragmentNode = (fragmentInfo: FragmentSelectionInfo) => {
      const { fragment, selection } = fragmentInfo

      if (fragment?.isSubType) {
        if (parsedSubTypeNames.indexOf(fragment.name) === -1) {
          parsedSubTypeNames.push(fragment.name)
        }
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
      const comprehensive =
        parsedSubTypeNames.length ===
        this.schema.getPossibleTypes(schemaRoot as GraphQLInterfaceType).length

      const fragmentKey =
        parsedSubTypes.length === 1 ? '__fragment' : '__fragments'

      if (fragmentKey in selectionSet) {
        throw new Error(`Please rename the "${fragmentKey}" field in query`)
      }

      return {
        ...selectionSet,
        [fragmentKey]: graphqlUnion(parsedSubTypes, comprehensive),
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
  ): FragmentSelectionInfo {
    const fragmentName = node.name.value
    this._fragments.push(fragmentName)

    return this.parseFragmentNode(this.allFragments[fragmentName], schemaRoot)
  }

  parseFragmentNode(
    fragment: { typeCondition?: NamedTypeNode; selectionSet: SelectionSetNode },
    schemaRoot: GraphQLSelectionSchema
  ): FragmentSelectionInfo {
    const { typeCondition, selectionSet } = fragment

    const fragmentName = typeCondition?.name.value

    if (!fragmentName) {
      return {
        fragment: null,
        selection: this.parseSelectionSetNode(selectionSet, schemaRoot),
      }
    }

    const fragmentSchema = assertObjectType(this.schema.getType(fragmentName))

    return {
      fragment: {
        name: fragmentName,
        isSubType:
          isAbstractType(schemaRoot) &&
          this.schema.isSubType(schemaRoot, fragmentSchema),
      },
      selection: this.parseSelectionSetNode(selectionSet, fragmentSchema),
    }
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
