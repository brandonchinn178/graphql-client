import { moduleToPath, pathToModule } from './path'

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
