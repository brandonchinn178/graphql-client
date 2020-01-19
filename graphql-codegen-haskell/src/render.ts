import * as Mustache from 'mustache'

import { PluginConfig } from './config'
import { ParsedEnum, ParsedOperation } from './parse/operation'
import { ParsedSelection, ParsedSelectionType } from './parse/selectionSet'
import { ParsedType } from './parse/variableDefinition'
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
      schema: operation.schema.replace(/\n/g, '\n  '),
      args: operation.args.map(({ name, type }) => ({
        arg: name,
        type: renderHaskellType(type),
      })),
      overArgs() {
        return (text: string) => templateOverList(text, this.args)
      },
    })),
  })

const renderHaskellType = (type: ParsedType): string => {
  const baseType = type.list ? `[${renderHaskellType(type.inner)}]` : type.name
  return type.nullable ? `Maybe ${baseType}` : baseType
}

export const renderAesonSchema = (selections: ParsedSelection): string => {
  const renderSelectionType = (selectionType: ParsedSelectionType): string => {
    if (selectionType.nullable) {
      const type = renderSelectionType({
        ...selectionType,
        nullable: false,
      })
      return `Maybe ${type}`
    }

    if (selectionType.list) {
      const type = renderSelectionType(selectionType.inner)
      return `List ${type}`
    }

    if ('name' in selectionType) {
      switch (selectionType.name) {
        case 'Float':
          return 'Double'
        case 'String':
          return 'Text'
        case 'Boolean':
          return 'Bool'
        case 'ID':
          return 'Text'
        default:
          return selectionType.name
      }
    }

    return renderAesonSchema(selectionType.fields).replace(/\n/g, '\n  ')
  }

  const fields = Object.entries(selections).map(
    ([name, selectionType]) =>
      `  ${name}: ${renderSelectionType(selectionType)},\n`
  )

  return '{\n' + fields.join('') + '}'
}
