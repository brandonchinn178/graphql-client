import * as Mustache from 'mustache'

import { PluginConfig } from '~/config'
import { ParsedEnum } from '~/parse/operation'
import { templateOverList } from '~/utils'

import enumTemplate from './templates/enum.mustache'
import enumParentTemplate from './templates/enumParent.mustache'

export const renderEnumModule = (
  config: PluginConfig,
  parsedEnum: ParsedEnum
): {
  enumModuleName: string
  enumModule: string
} => {
  const enumModuleName = `${config.enumsModule}.${parsedEnum.name}`
  const enumModule = Mustache.render(enumTemplate, {
    ...config,
    ...parsedEnum,
    enumModuleName,
    overValues() {
      return (text: string) => templateOverList(text, this.values)
    },
  })
  return { enumModuleName, enumModule }
}

export const renderEnumParentModule = (
  config: PluginConfig,
  enumModules: readonly string[]
) =>
  Mustache.render(enumParentTemplate, {
    ...config,
    enumModules,
  })
