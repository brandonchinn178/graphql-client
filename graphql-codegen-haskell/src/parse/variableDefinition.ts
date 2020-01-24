import { Kind, TypeNode, VariableDefinitionNode } from 'graphql'

import {
  graphqlList,
  graphqlScalar,
  ParsedListType,
  ParsedScalarType,
} from './graphqlTypes'

export type ParsedVariableDefinitions = {
  // The name of the argument
  name: string
  // The Haskell type of the argument
  type: ParsedType
}

export const parseVariableDefinitions = (
  variableDefinitions: ReadonlyArray<VariableDefinitionNode>
): ParsedVariableDefinitions[] =>
  variableDefinitions.map(({ type, variable }) => ({
    name: variable.name.value,
    type: parseType(type),
  }))

export type ParsedType = ParsedScalarType | ParsedListType<ParsedType>

const parseType = (type: TypeNode, nullable = true): ParsedType => {
  switch (type.kind) {
    case Kind.NAMED_TYPE:
      return graphqlScalar(type.name.value, nullable)
    case Kind.LIST_TYPE:
      return graphqlList(parseType(type.type), nullable)
    case Kind.NON_NULL_TYPE:
      return parseType(type.type, false)
  }
}
