import * as Mustache from 'mustache'

import { PluginConfig } from '../config'
import { ParsedEnum, ParsedOperation } from '../parse/operation'
import { ParsedSelection, ParsedSelectionType } from '../parse/selectionSet'
import { ParsedType } from '../parse/variableDefinition'
import { templateOverList } from '../utils'
import template from './template.mustache'

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
      query: indent(operation.query),
      schema: indent(renderAesonSchema(operation.schema)),
      args: operation.args.map(({ name, type }) => ({
        arg: name,
        type: renderHaskellType(type),
      })),
      overArgs() {
        return (text: string) => templateOverList(text, this.args)
      },
    })),
  })

export const renderHaskellType = (type: ParsedType): string => {
  const baseType = type.list
    ? `[${renderHaskellType(type.inner)}]`
    : renderHaskellScalarType(type.name)

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
      return renderHaskellScalarType(selectionType.name)
    }

    if ('subTypes' in selectionType) {
      return indent(
        selectionType.subTypes
          .map((selection) => renderAesonSchema(selection))
          .join(' | ')
      )
    }

    return indent(renderAesonSchema(selectionType.fields))
  }

  const fields = Object.entries(selections).map(
    ([name, selectionType]) => `${name}: ${renderSelectionType(selectionType)}`
  )

  return '{\n' + fields.map((field) => `  ${field},`).join('\n') + '\n}'
}

const indent = (s: string) => s.replace(/\n/g, '\n  ')

const renderHaskellScalarType = (name: string) => {
  switch (name) {
    case 'Float':
      return 'Double'
    case 'String':
    case 'ID':
      return 'Text'
    case 'Boolean':
      return 'Bool'
    default:
      return name
  }
}
