import { validateConfig } from './config'

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
