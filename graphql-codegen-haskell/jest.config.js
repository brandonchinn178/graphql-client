const jest = require('ts-jest/utils')

const tsconfig = require('./tsconfig')

module.exports = {
  testEnvironment: 'node',
  transform: {
    '^.+\\.ts$': 'ts-jest',
    '\\.mustache$': 'jest-raw-loader',
  },
  moduleNameMapper: {
    ...jest.pathsToModuleNameMapper(tsconfig.compilerOptions.paths, {
      prefix: '<rootDir>/',
    }),
  },
  globals: {
    'ts-jest': {
      tsConfig: {
        // https://github.com/kulshekhar/ts-jest/issues/777
        // https://itnext.io/great-import-schism-typescript-confusion-around-imports-explained-d512fc6769c2
        esModuleInterop: true,
      },
    },
  },
}
