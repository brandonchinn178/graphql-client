import { resolveConfig, validateConfig } from './config'

const fullConfig = {
  hsSrcDir: 'src/',
  enumsModule: 'Exammple.GraphQL.Enums',
  scalarsModule: 'Example.GraphQL.Scalarjs',
}

describe('validateConfig', () => {
  it('validates a valid config', () => {
    expect(() => validateConfig(fullConfig)).not.toThrow()
  })

  it('requires enumsModule', () => {
    const config = { ...fullConfig }
    delete config.enumsModule
    expect(() => validateConfig(config)).toThrow()
  })

  it('requires scalarModule', () => {
    const config = { ...fullConfig }
    delete config.scalarsModule
    expect(() => validateConfig(config)).toThrow()
  })

  it('does not require hsSrcDir', () => {
    const config = { ...fullConfig }
    delete config.hsSrcDir
    expect(() => validateConfig(config)).not.toThrow()
  })
})

describe('resolveConfig', () => {
  it('parses hsSrcDir and apiModule from the output file', () => {
    const config = { ...fullConfig }
    delete config.hsSrcDir
    expect(
      resolveConfig(config, 'foo/src/Example/GraphQL/API.hs')
    ).toMatchObject({
      hsSrcDir: 'foo/src',
      apiModule: 'Example.GraphQL.API',
    })
  })

  it('parses apiModule from the output file without inferrable hsSrcDir', () => {
    const config = { ...fullConfig }
    delete config.hsSrcDir
    expect(resolveConfig(config, 'Example/GraphQL/API.hs')).toMatchObject({
      hsSrcDir: '',
      apiModule: 'Example.GraphQL.API',
    })
  })

  it('parses hsSrcDir and apiModule from a provided hsSrcDir', () => {
    const config = {
      ...fullConfig,
      hsSrcDir: 'src/Example/',
    }
    expect(resolveConfig(config, 'src/Example/GraphQL/API.hs')).toMatchObject({
      hsSrcDir: 'src/Example/',
      apiModule: 'GraphQL.API',
    })
  })

  it('allows specifying hsSrcDir without trailing slash', () => {
    const config = {
      ...fullConfig,
      hsSrcDir: 'src/Example',
    }
    expect(resolveConfig(config, 'src/Example/GraphQL/API.hs')).toMatchObject({
      hsSrcDir: 'src/Example',
      apiModule: 'GraphQL.API',
    })
  })
})
