import {
  GraphQLSchema,
  isEnumType,
  Kind,
  TypeNode,
  VariableDefinitionNode,
} from 'graphql'

import {
  graphqlList,
  graphqlScalar,
  ParsedListType,
  ParsedScalarType,
} from './graphqlTypes'

export type ParsedVariableDefinitions = {
  args: ParsedArgument[]
  enums: Set<string>
}

export type ParsedArgument = {
  // The name of the argument
  name: string
  // The Haskell type of the argument
  type: ParsedType
}

export const parseVariableDefinitions = (
  schema: GraphQLSchema,
  variableDefinitions: ReadonlyArray<VariableDefinitionNode>
): ParsedVariableDefinitions => {
  const args = variableDefinitions.map(({ type, variable }) => ({
    name: variable.name.value,
    type: parseType(type),
  }))

  const enums = new Set<string>()
  args.forEach(({ type }) => {
    const enumName = getEnumName(schema, type)
    if (enumName) {
      enums.add(enumName)
    }
  })

  return { args, enums }
}

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

const getEnumName = (
  schema: GraphQLSchema,
  type: ParsedType
): string | null => {
  if (type.list) {
    return getEnumName(schema, type.inner)
  }

  const schemaType = schema.getType(type.name)
  return isEnumType(schemaType) ? type.name : null
}
