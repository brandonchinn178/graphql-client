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
    apiModule: pathToModule(outputFile),
    ...rawConfig,
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

/**
 * A mustache function that transforms
 *
 * [ {{foo}} ~ {{bar}}
 * ]
 *
 * with [{ foo: 'a', bar: 1 }, { foo: 'b', bar: 2 }] into
 *
 * [ a ~ 1
 * , b ~ 2
 * ]
 */
const templateOverList = (
  text: string,
  list: Array<{ [key: string]: string }>
) => {
  const match = text.match(/^\s*\n(.*?)\n(.*?)\n\s*$/)
  if (!match) {
    throw new Error('templateOverList requires a two-line template')
  }

  const render = (template: string, vars: { [key: string]: string }): string =>
    template.replace(/{{(.*?)}}/g, (_, name) => vars[name])

  const templateFirst = match[1]
  const templateRest = templateFirst.replace(/[^\s]/, ',')
  const suffix = match[2]

  const listFirst = list[0]
  const listRest = list.slice(1)

  const lines = ([] as string[]).concat(
    render(templateFirst, listFirst),
    listRest.map((elem) => render(templateRest, elem)),
    suffix
  )

  return lines.map((s) => s + '\n').join('')
}
