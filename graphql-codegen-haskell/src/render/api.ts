import * as Mustache from 'mustache'

import { PluginConfig } from '~/config'
import { ParsedOperation } from '~/parse/operation'
import { ParsedSelection, ParsedSelectionType } from '~/parse/selectionSet'
import { ParsedType } from '~/parse/variableDefinition'
import { templateOverList } from '~/utils'

import template from './templates/api.mustache'

export const renderAPIModule = (
  config: PluginConfig,
  enumModules: readonly string[],
  operations: readonly ParsedOperation[]
) =>
  Mustache.render(template, {
    ...config,
    enumModules,
    operations: operations.map((operation) => ({
      ...operation,
      query: indent(operation.query),
      schema: indent(renderAesonSchema(operation.schema)),
      args: operation.args.map(({ name, type }) => ({
        arg: name,
        type: renderHaskellType(type),
      })),
      overArgs() {
        return (text: string) =>
          templateOverList(text, this.args, { context: this })
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
  const fields = Object.entries(selections).map(
    ([name, selectionType]) => `${name}: ${renderSelectionType(selectionType)}`
  )

  return '{\n' + fields.map((field) => `  ${field},`).join('\n') + '\n}'
}

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
    const unionSchemas = selectionType.subTypes.map((selection) =>
      renderAesonSchema(selection)
    )

    const sumType = indent(
      '(' + indent('\n' + unionSchemas.join(' |\n')) + '\n)'
    )

    return selectionType.comprehensive ? sumType : `Try ${sumType}`
  }

  return indent(renderAesonSchema(selectionType.fields))
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
