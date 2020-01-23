import { pathToModule, resolveConfig, validateConfig } from './config'

const fullConfig = {
  apiModule: 'Example.GraphQL.API',
  scalarsModule: 'Example.GraphQL.Scalars',
}

describe('validateConfig', () => {
  it('validates a valid config', () => {
    expect(() => validateConfig(fullConfig)).not.toThrow()
  })

  it('requires scalarModule', () => {
    const config = { ...fullConfig }
    delete config.scalarsModule
    expect(() => validateConfig(config)).toThrow()
  })

  it('does not require apiModule', () => {
    const config = { ...fullConfig }
    delete config.apiModule
    expect(() => validateConfig(config)).not.toThrow()
  })
})

describe('pathToModule', () => {
  it.each`
    filepath                                | moduleName
    ${'MyModule/GraphQL/API.hs'}            | ${'MyModule.GraphQL.API'}
    ${'foo/bar/src/Example/GraphQL/API.hs'} | ${'Example.GraphQL.API'}
  `('$filepath => $moduleName', ({ filepath, moduleName }) => {
    expect(pathToModule(filepath)).toBe(moduleName)
  })
})

describe('resolveConfig', () => {
  it('provides a default for apiModule', () => {
    const config = { ...fullConfig }
    delete config.apiModule

    expect(resolveConfig(config, 'path/to/MyModule/API.hs')).toMatchObject({
      apiModule: 'MyModule.API',
    })
  })

  it('keeps a set apiModule', () => {
    expect(resolveConfig(fullConfig, 'path/to/MyModule/API.hs')).toMatchObject({
      apiModule: fullConfig.apiModule,
    })
  })
})
