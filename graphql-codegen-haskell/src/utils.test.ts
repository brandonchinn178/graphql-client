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
