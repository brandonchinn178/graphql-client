import {
  PluginFunction,
  PluginValidateFn,
} from '@graphql-codegen/plugin-helpers'
import { concatAST } from 'graphql'

import { RawPluginConfig, resolveConfig, validateConfig } from './config'
import { parseFragments } from './parse/fragments'
import { parseOperations } from './parse/operation'
import { renderAPIModule } from './render/api'
import { renderEnumModule } from './render/enum'
import { compact, moduleToPath, writeFile } from './utils'

export const plugin: PluginFunction<RawPluginConfig> = (
  schema,
  documents,
  rawConfig,
  info = {}
) => {
  const { outputFile = '' } = info

  const config = resolveConfig(rawConfig, outputFile)

  const ast = concatAST(compact(documents.map((v) => v.document)))

  const parsedFragments = parseFragments(ast)
  const { enums, operations } = parseOperations(ast, schema, parsedFragments)
  const enumModules = [] as string[]

  // Generate the module for each enum
  enums.forEach((parsedEnum) => {
    const { enumModuleName, enumModule } = renderEnumModule(config, parsedEnum)

    const enumModulePath = moduleToPath(enumModuleName, config.hsSrcDir)
    writeFile(enumModulePath, enumModule)

    enumModules.push(enumModuleName)
  })

  return renderAPIModule(config, enumModules, operations)
}

export const validate: PluginValidateFn = (_schema, _documents, config) => {
  validateConfig(config)
}
