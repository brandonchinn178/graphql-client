import commonjs from '@rollup/plugin-commonjs'
import nodeResolve from '@rollup/plugin-node-resolve'
import externals from 'rollup-plugin-node-externals'
import { string } from 'rollup-plugin-string'
import typescript from 'rollup-plugin-typescript2'

export default {
  input: './src/index.ts',
  output: [
    {
      file: '../graphql-client/js/graphql-codegen-haskell.js',
      format: 'cjs',
    },
  ],
  external: [
    // Libraries that shouldn't be bundled. Each of these should
    // have a corresponding file in graphql-client/js/mocks/.
    'websocket',
  ],
  plugins: [
    nodeResolve(),
    commonjs({
      // These are optional dependencies imported via
      // `try { require(...) } catch (e) { ... }` blocks, so we should
      // leave as `require(...)` and not hoist.
      // https://github.com/rollup/rollup-plugin-commonjs/issues/355
      ignore: ['bufferutil', 'utf-8-validate'],
    }),
    externals({
      peerDeps: false,
      optDeps: false,
      devDeps: false,
    }),

    typescript({
      typescript: require('typescript'),
      tsconfigFile: 'tsconfig.build.json',
    }),

    string({
      include: '**/*.mustache',
    }),
  ],
}
