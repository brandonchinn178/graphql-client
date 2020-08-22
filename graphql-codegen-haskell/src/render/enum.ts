import * as Mustache from 'mustache'

import { PluginConfig } from '~/config'
import { ParsedEnum } from '~/parse/operation'
import { templateOverList } from '~/utils'

import template from './templates/enum.mustache'

export const renderEnumModule = (
  config: PluginConfig,
  parsedEnum: ParsedEnum,
  enumModuleName: string
): string =>
  Mustache.render(template, {
    ...config,
    ...parsedEnum,
    enumModuleName,
    overValues() {
      return (text: string) => templateOverList(text, this.values)
    },
  })
