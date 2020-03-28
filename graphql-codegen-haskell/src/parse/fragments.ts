import { DocumentNode, FragmentDefinitionNode, Kind } from 'graphql'

import { mergeObjects } from '~/utils'

export type ParsedFragments = Record<string, FragmentDefinitionNode>

export const parseFragments = (ast: DocumentNode): ParsedFragments => {
  const fragments = ast.definitions.filter(
    ({ kind }) => kind === Kind.FRAGMENT_DEFINITION
  ) as FragmentDefinitionNode[]

  return mergeObjects(
    fragments.map((fragment) => ({ [fragment.name.value]: fragment }))
  )
}
