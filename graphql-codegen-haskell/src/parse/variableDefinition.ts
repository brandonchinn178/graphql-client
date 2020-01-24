import { Kind, TypeNode, VariableDefinitionNode } from 'graphql'

import { ParsedListType, ParsedScalarType } from './graphqlTypes'

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

const parseType = (type: TypeNode): ParsedType => {
  switch (type.kind) {
    case Kind.NAMED_TYPE:
      return {
        list: false,
        name: type.name.value,
        nullable: true,
      }
    case Kind.LIST_TYPE:
      return {
        list: true,
        inner: parseType(type.type),
        nullable: true,
      }
    case Kind.NON_NULL_TYPE:
      return {
        ...parseType(type.type),
        nullable: false,
      }
  }
}