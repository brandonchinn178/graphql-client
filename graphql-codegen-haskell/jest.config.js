module.exports = {
  testEnvironment: 'node',
  transform: {
    '^.+\\.ts$': 'ts-jest',
    '\\.mustache$': 'jest-raw-loader',
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
