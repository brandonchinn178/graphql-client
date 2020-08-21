import commonjs from '@rollup/plugin-commonjs'
import { nodeResolve } from '@rollup/plugin-node-resolve'
import { string } from 'rollup-plugin-string'
import typescript from 'rollup-plugin-typescript2'

export default {
  input: './src/index.ts',
  output: [
    {
      file: './dist/index.js',
      format: 'cjs',
    },
  ],
  external: ['fs', 'path'],
  plugins: [
    nodeResolve(),
    commonjs(),

    typescript({
      typescript: require('typescript'),
      tsconfigFile: 'tsconfig.build.json',
    }),

    string({
      include: '**/*.mustache',
    }),
  ],
}
