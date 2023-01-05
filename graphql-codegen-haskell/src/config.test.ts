import * as _ from 'lodash'

import { validateConfig } from './config'

const fullConfig = {
  schema: 'https://graphbrainz.fly.dev',
  documents: ['**/*.graphql'],

  hsSourceDir: 'foo/bar/',

  apiModule: 'Example.GraphQL.API',
  enumsModule: 'Example.GraphQL.Enums',
  scalarsModule: 'Example.GraphQL.Scalars',
}

describe('validateConfig', () => {
  it('validates a valid config', () => {
    expect(validateConfig(fullConfig)).toEqual(fullConfig)
  })

  describe('schema', () => {
    it('is required', () => {
      const config = _.omit(fullConfig, 'schema')
      expect(() => validateConfig(config)).toThrow()
    })
  })

  describe('documents', () => {
    it('is required', () => {
      const config = _.omit(fullConfig, 'documents')
      expect(() => validateConfig(config)).toThrow()
    })

    it('works on an array of one path', () => {
      const config = {
        ...fullConfig,
        documents: ['dir1/*.graphql'],
      }
      expect(validateConfig(config)).toEqual(config)
    })

    it('works on an array of multiple paths', () => {
      const config = {
        ...fullConfig,
        documents: ['dir1/*.graphql', 'dir2/*.graphql'],
      }
      expect(validateConfig(config)).toEqual(config)
    })

    it('casts a single path into an array', () => {
      const config = {
        ...fullConfig,
        documents: 'dir1/*.graphql',
      }
      expect(validateConfig(config)).toEqual({
        ...config,
        documents: ['dir1/*.graphql'],
      })
    })
  })

  describe('hsSourceDir', () => {
    it('defaults to src/', () => {
      const config = _.omit(fullConfig, 'hsSourceDir')
      expect(validateConfig(config)).toEqual({
        ...fullConfig,
        hsSourceDir: 'src/',
      })
    })
  })

  describe('apiModule', () => {
    it('is required', () => {
      const config = _.omit(fullConfig, 'apiModule')
      expect(() => validateConfig(config)).toThrow()
    })
  })

  describe('enumsModule', () => {
    it('is required', () => {
      const config = _.omit(fullConfig, 'enumsModule')
      expect(() => validateConfig(config)).toThrow()
    })
  })

  describe('scalarsModule', () => {
    it('is required', () => {
      const config = _.omit(fullConfig, 'scalarsModule')
      expect(() => validateConfig(config)).toThrow()
    })
  })
})
