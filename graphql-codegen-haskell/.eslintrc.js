module.exports = {
  parser: '@typescript-eslint/parser',
  plugins: ['@typescript-eslint', 'simple-import-sort', 'jest'],
  extends: [
    'eslint:recommended',
    'plugin:jest/recommended',
    'plugin:@typescript-eslint/recommended',
    'plugin:prettier/recommended',
  ],
  env: {
    node: true,
  },
  parserOptions: {
    ecmaVersion: 6,
    sourceType: 'module',
  },
  rules: {
    'arrow-parens': 'off',
    'block-scoped-var': 'error',
    'class-methods-use-this': 'error',
    'comma-dangle': 'off',
    'curly': 'off',
    'eqeqeq': 'error',
    'indent': 'off',
    'max-classes-per-file': ['error', 1],
    'max-line-length': 'off',
    'no-array-constructor': 'error',
    'no-async-promise-executor': 'error',
    'no-await-in-loop': 'error',
    'no-console': [
      'error',
      {
        allow: ['warn', 'error'],
      },
    ],
    // https://github.com/typescript-eslint/typescript-eslint/issues/291
    'no-dupe-class-members': 'off',
    'no-dupe-keys': 'error',
    'no-duplicate-case': 'error',
    'no-empty': ['error', { allowEmptyCatch: true }],
    'no-empty-character-class': 'error',
    'no-import-assign': 'error',
    'no-invalid-regexp': 'error',
    'no-irregular-whitespace': 'error',
    'no-lone-blocks': 'error',
    'no-new-func': 'error',
    'no-new-object': 'error',
    'no-octal': 'error',
    'no-param-reassign': 'error',
    'no-return-await': 'error',
    'no-throw-literal': 'error',
    'no-unmodified-loop-condition': 'error',
    'no-useless-catch': 'error',
    'no-useless-escape': 'error',
    'no-useless-return': 'error',
    'radix': 'error',
    'require-await': 'error',
    'semi': ['error', 'never'],
    'space-in-parens': ['error', 'never'],
    'sort-imports': 'off',
    'simple-import-sort/sort': [
      'error',
      {
        groups: [
          // Side effect imports.
          ['^\\u0000'],
          // Packages.
          // Things that start with a letter (or digit or underscore), or `@` followed by a letter.
          ['^@?\\w'],
          // Absolute imports and other imports such as Vue-style `@/foo`.
          // Anything that does not start with a dot.
          ['^[^.]'],
          // Relative imports.
          // Anything that starts with a dot.
          ['^\\.'],
        ],
      },
    ],
    '@typescript-eslint/ban-ts-ignore': 'error',
    '@typescript-eslint/camelcase': 'off',
    '@typescript-eslint/explicit-function-return-type': 'off',
    '@typescript-eslint/member-delimiter-style': [
      'error',
      {
        multiline: {
          delimiter: 'none',
        },
      },
    ],
    '@typescript-eslint/no-explicit-any': 'error',
    '@typescript-eslint/no-non-null-assertion': 'error',
    '@typescript-eslint/no-unused-vars': ['error', { argsIgnorePattern: "^_" }],
    '@typescript-eslint/no-use-before-define': 'off',
    '@typescript-eslint/no-var-requires': 'off',
  },
}
