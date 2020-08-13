/* eslint-disable-next-line @typescript-eslint/no-explicit-any */
type Context = Record<string, any>

type TemplateOptions = {
  sep?: string
  context?: Context
}

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
export const templateOverList = <T>(
  text: string,
  list: Array<T>,
  options: TemplateOptions = {}
): string => {
  const { sep = ',', context = {} } = options

  /* eslint-disable-next-line @typescript-eslint/no-explicit-any */
  const getKey = (elem: Context, key: string): any => elem[key] ?? context[key]

  /* eslint-disable-next-line @typescript-eslint/no-explicit-any */
  const render = (template: string, elem: any): string =>
    template
      .replace(/{{\.}}/g, (_) => elem)
      .replace(/{{(.*?)}}/g, (_, name) => getKey(elem, name))

  const templateFirst = removeEmptyLines(text)
  const templateRest = templateFirst.replace(/[^\s]/, sep)

  if (list.length === 0) {
    return templateFirst.replace(/(\s*[^\s]).*/, '$1') + '\n'
  }

  const listFirst = list[0]
  const listRest = list.slice(1)

  const lines = ([] as string[]).concat(
    render(templateFirst, listFirst),
    listRest.map((elem) => render(templateRest, elem))
  )

  return lines.map((s) => s + '\n').join('')
}

/**
 * Remove empty lines in the given string.
 */
const removeEmptyLines = (s: string) =>
  s
    .split('\n')
    .filter((s) => !s.match(/^\s*$/))
    .join('\n')
