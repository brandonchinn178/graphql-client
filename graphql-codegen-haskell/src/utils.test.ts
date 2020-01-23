import { mergeObjects, templateOverList } from './utils'

describe('mergeObjects', () => {
  it('merges simple objects', () => {
    expect(mergeObjects([{ a: 1 }, { b: 2 }, { c: 3 }])).toEqual({
      a: 1,
      b: 2,
      c: 3,
    })
  })

  it('keeps keys in latter objects', () => {
    expect(mergeObjects([{ a: 1 }, { b: 2 }, { a: 3 }])).toEqual({
      a: 3,
      b: 2,
    })
  })
})

describe('templateOverList', () => {
  it('requires a two-line template', () => {
    expect(() => templateOverList('[ {{foo}}', [])).toThrow()
    expect(() => templateOverList('[ {{foo}}\n]\n!\n', [])).toThrow()
  })

  it('interpolates elements directly', () => {
    const result = templateOverList('[ {{.}}\n]', ['a', 'b', 'c'])
    expect(result).toMatchInlineSnapshot(`
      "[ a
      , b
      , c
      ]
      "
    `)
  })

  it('interpolates object keys', () => {
    const result = templateOverList('[ {{foo}} ~ {{bar}}\n]', [
      { foo: 'a', bar: 1 },
      { foo: 'b', bar: 2 },
    ])
    expect(result).toMatchInlineSnapshot(`
      "[ a ~ 1
      , b ~ 2
      ]
      "
    `)
  })
})
