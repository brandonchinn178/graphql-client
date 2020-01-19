import {
  DocumentNode,
  FragmentDefinitionNode,
  Kind,
  SelectionSetNode,
} from 'graphql'

import { mergeObjects } from '../utils'

export type ParsedFragments = Record<string, ParsedFragment>

export type ParsedFragment = {
  name: string
  selectionSet: SelectionSetNode
  node: FragmentDefinitionNode
}

export const parseFragments = (ast: DocumentNode): ParsedFragments => {
  const fragments = ast.definitions.filter(
    ({ kind }) => kind === Kind.FRAGMENT_DEFINITION
  ) as FragmentDefinitionNode[]

  return mergeObjects(
    fragments.map((node) => {
      const name = node.name.value
      const fragment = {
        name,
        selectionSet: node.selectionSet,
        node,
      }

      return {
        [name]: fragment,
      }
    })
  )
}
