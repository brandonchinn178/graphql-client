import { string } from 'rollup-plugin-string'
import typescript from 'rollup-plugin-typescript2'

import pkg from './package.json'

export default {
  input: './src/index.ts',
  output: [
    {
      file: './dist/index.js',
      format: 'cjs',
    },
  ],
  external: [
    ...Object.keys(pkg.dependencies || {}),
    ...Object.keys(pkg.peerDependencies || {}),
  ],
  plugins: [
    typescript({
      typescript: require('typescript'),
      tsconfigFile: 'tsconfig.build.json',
    }),

    string({
      include: '**/*.mustache',
    }),
  ],
}
