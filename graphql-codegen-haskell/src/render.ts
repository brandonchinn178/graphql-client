import * as Mustache from 'mustache'

import { PluginConfig } from './config'
import { ParsedEnum, ParsedOperation, ParsedType } from './parse'
import template from './template.mustache'
import { templateOverList } from './utils'

export const renderAPIModule = (
  config: PluginConfig,
  enums: readonly ParsedEnum[],
  operations: readonly ParsedOperation[]
) =>
  Mustache.render(template, {
    ...config,
    enums: enums.map((parsedEnum) => ({
      ...parsedEnum,
      overValues() {
        return (text: string) => templateOverList(text, this.values)
      },
    })),
    operations: operations.map((operation) => ({
      ...operation,
      query: operation.query.replace(/\n/g, '\n  '),
      overArgs() {
        return (text: string) => templateOverList(text, this.args)
      },
    })),
  })

export const renderHaskellType = (type: ParsedType): string => {
  const baseType = type.list ? `[${renderHaskellType(type.inner)}]` : type.name
  return type.nullable ? `Maybe ${baseType}` : baseType
}
