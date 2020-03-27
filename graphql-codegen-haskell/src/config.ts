import * as yup from 'yup'

import { pathToModule } from './utils'

const CONFIG_SCHEMA = yup.object({
  // The directory where Haskell source files live. By default tries to infer it
  // from the output file path.
  hsSrcDir: yup.string().notRequired(),

  // The Haskell module containing all the scalar definitions
  scalarsModule: yup.string().required(),
})

export type RawPluginConfig = yup.InferType<typeof CONFIG_SCHEMA>

export const validateConfig = (config: { [key: string]: unknown }) => {
  CONFIG_SCHEMA.validateSync(config)
}

export type PluginConfig = RawPluginConfig & {
  hsSrcDir: NonNullable<RawPluginConfig['hsSrcDir']>
  apiModule: string
}

export const resolveConfig = (
  config: RawPluginConfig,
  outputFile: string
): PluginConfig => {
  const hsSrcDir = config.hsSrcDir ?? inferSrcDir(outputFile)
  const apiModule = pathToModule(outputFile, hsSrcDir)

  return {
    ...config,
    hsSrcDir,
    apiModule,
  }
}

// Infer the hs-source-dir from the given Haskell module path. e.g.
// "src/Example/GraphQL/API.hs" => "src/"
const inferSrcDir = (path: string) => {
  const parts = path.split('/')
  const result = []
  for (const part of parts) {
    if (!/^[A-Z]/.test(part)) {
      result.push(part)
    } else {
      break
    }
  }
  return result.join('/')
}
