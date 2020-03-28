import { templateOverList } from './template'

describe('templateOverList', () => {
  it('renders no list', () => {
    const result = templateOverList('[ {{.}}', [])
    expect(result).toMatchInlineSnapshot(`
      "[
      "
    `)
  })

  it('interpolates elements directly', () => {
    const result = templateOverList('[ {{.}}', ['a', 'b', 'c'])
    expect(result).toMatchInlineSnapshot(`
      "[ a
      , b
      , c
      "
    `)
  })

  it('interpolates with a different delimiter', () => {
    const result = templateOverList('* {{.}}', ['a', 'b', 'c'], '|')
    expect(result).toMatchInlineSnapshot(`
      "* a
      | b
      | c
      "
    `)
  })

  it('renders multiline templates', () => {
    const result = templateOverList('* {{.}}\n  > {{.}}!', ['a', 'b', 'c'])
    expect(result).toMatchInlineSnapshot(`
      "* a
        > a!
      , b
        > b!
      , c
        > c!
      "
    `)
  })

  it('interpolates object keys', () => {
    const result = templateOverList('[ {{foo}} ~ {{bar}}', [
      { foo: 'a', bar: 1 },
      { foo: 'b', bar: 2 },
    ])
    expect(result).toMatchInlineSnapshot(`
      "[ a ~ 1
      , b ~ 2
      "
    `)
  })
})
