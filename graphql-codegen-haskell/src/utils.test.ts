import {
  mergeObjects,
  moduleToPath,
  pathToModule,
  templateOverList,
} from './utils'

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

describe('pathToModule', () => {
  it('converts (src/Foo/Bar.hs, src/) => Foo.Bar', () => {
    expect(pathToModule('src/Foo/Bar.hs', 'src/')).toBe('Foo.Bar')
  })

  it('works without trailing slash in srcDir', () => {
    expect(pathToModule('src/Foo/Bar.hs', 'src')).toEqual(
      pathToModule('src/Foo/Bar.hs', 'src/')
    )
  })

  it('works with no srcDir', () => {
    expect(pathToModule('Foo/Bar.hs', '')).toBe('Foo.Bar')
  })

  it('errors if srcDir is not a prefix of path', () => {
    expect(() => pathToModule('src/Foo/Bar.hs', 'lib/')).toThrow()
  })

  it('looks for complete srcDir match', () => {
    expect(() => pathToModule('my-project/Foo/Bar.hs', 'my-proj')).toThrow()
  })
})

describe('moduleToPath', () => {
  it('converts (Foo.Bar, src/) => src/Foo/Bar.hs', () => {
    expect(moduleToPath('Foo.Bar', 'src/')).toBe('src/Foo/Bar.hs')
  })

  it('works without trailing slash in srcDir', () => {
    expect(moduleToPath('Foo.Bar', 'src')).toEqual(
      moduleToPath('Foo.Bar', 'src/')
    )
  })

  it('works with no srcDir', () => {
    expect(moduleToPath('Foo.Bar', '')).toBe('Foo/Bar.hs')
  })
})
