import {
  PluginFunction,
  PluginValidateFn,
} from '@graphql-codegen/plugin-helpers'
import { concatAST, visit } from 'graphql'

import { PluginConfig, validateConfig } from './config'
import { parseFragments } from './parse/fragments'
import { renderAPIModule } from './render'
import { GraphQLHaskellVisitor } from './visitor'

export const plugin: PluginFunction<PluginConfig> = (
  schema,
  documents,
  rawConfig,
  info = {}
) => {
  const { outputFile = '' } = info

  const config = {
    apiModule: pathToModule(outputFile),
    ...rawConfig,
  }

  const ast = concatAST(documents.map(({ content }) => content))

  const parsedFragments = parseFragments(ast)

  const visitor = new GraphQLHaskellVisitor(schema, parsedFragments, config)
  visit(ast, { leave: visitor })

  return renderAPIModule(config, visitor.getEnums(), visitor.getOperations())
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
