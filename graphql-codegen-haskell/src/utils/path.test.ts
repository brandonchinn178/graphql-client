import { moduleToPath } from './path'

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
