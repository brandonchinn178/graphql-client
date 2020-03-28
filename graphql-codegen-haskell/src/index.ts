import {
  PluginFunction,
  PluginValidateFn,
} from '@graphql-codegen/plugin-helpers'
import * as fs from 'fs'
import { concatAST } from 'graphql'
import * as path from 'path'

import { RawPluginConfig, resolveConfig, validateConfig } from './config'
import { parseFragments } from './parse/fragments'
import { parseOperations } from './parse/operation'
import { renderAPIModule } from './render/api'
import { renderEnumModule } from './render/enum'
import { moduleToPath } from './utils'

export const plugin: PluginFunction<RawPluginConfig> = (
  schema,
  documents,
  rawConfig,
  info = {}
) => {
  const { outputFile = '' } = info

  const config = resolveConfig(rawConfig, outputFile)

  const ast = concatAST(documents.map(({ content }) => content))

  const parsedFragments = parseFragments(ast)
  const { enums, operations } = parseOperations(ast, schema, parsedFragments)

  const renderedEnums = enums.map((parsedEnum) =>
    renderEnumModule(config, parsedEnum)
  )

  renderedEnums.forEach(({ enumModuleName, enumModule }) => {
    const enumModulePath = moduleToPath(enumModuleName, config.hsSrcDir)

    try {
      fs.mkdirSync(path.dirname(enumModulePath), { recursive: true })
    } catch (_) {}

    fs.writeFileSync(enumModulePath, enumModule)
  })

  const enumModules = renderedEnums.map(({ enumModuleName }) => enumModuleName)
  return renderAPIModule(config, enumModules, operations)
}

export const validate: PluginValidateFn = (_schema, _documents, config) => {
  validateConfig(config)
}
