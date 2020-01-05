import {
  PluginFunction,
  PluginValidateFn,
} from '@graphql-codegen/plugin-helpers'
import { concatAST, FragmentDefinitionNode, Kind, visit } from 'graphql'
import * as Mustache from 'mustache'

import { PluginConfig, validateConfig } from './config'
import template from './template.mustache'
import { GraphQLHaskellVisitor } from './visitor'

export const plugin: PluginFunction<PluginConfig> = (
  schema,
  documents,
  rawConfig,
  info = {}
) => {
  const { outputFile = '' } = info

  const config = {
    ...rawConfig,
    apiModule: pathToModule(outputFile),
  }

  const ast = concatAST(documents.map(({ content }) => content))

  const fragments = ast.definitions.filter(
    ({ kind }) => kind === Kind.FRAGMENT_DEFINITION
  ) as FragmentDefinitionNode[]

  const visitor = new GraphQLHaskellVisitor(schema, fragments, config)
  visit(ast, { leave: visitor })

  return Mustache.render(template, {
    ...config,
    enums: visitor.getEnums().map((parsedEnum) => ({
      ...parsedEnum,
      overValues() {
        return (text: string) => templateOverList(text, this.values)
      },
    })),
    operations: visitor.getOperations().map((operation) => ({
      ...operation,
      query: operation.query.replace(/\n/g, '\n  '),
      overArgs() {
        return (text: string) => templateOverList(text, this.args)
      },
    })),
  })
}

export const validate: PluginValidateFn = (_schema, _documents, config) => {
  validateConfig(config)
}

// Convert "src/Example/GraphQL/API.hs" to "Example.GraphQL.API"
const pathToModule = (path: string) =>
  path
    .replace(/(^|.*?\/)(?=[A-Z])/, '')
    .replace(/\//g, '.')
    .replace(/\.hs$/, '')

const templateOverList = (
  text: string,
  list: Array<{ [key: string]: unknown }>
) => {
  // TODO
  return text
}
