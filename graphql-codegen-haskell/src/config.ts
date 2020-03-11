import * as yup from 'yup'

const CONFIG_SCHEMA = yup.object({
  // The Haskell module being written. By default will parse the output file
  // path
  apiModule: yup.string(),

  // The Haskell module containing all the scalar definitions
  scalarsModule: yup.string().required(),
})

export type RawPluginConfig = yup.InferType<typeof CONFIG_SCHEMA>

export const validateConfig = (config: { [key: string]: unknown }) => {
  CONFIG_SCHEMA.validateSync(config)
}

export type PluginConfig = {
  apiModule: string
  scalarsModule: string
}

export const resolveConfig = (
  config: RawPluginConfig,
  outputFile: string
): PluginConfig => {
  return {
    apiModule: pathToModule(outputFile),
    ...config,
  }
}

// Convert "src/Example/GraphQL/API.hs" to "Example.GraphQL.API"
export const pathToModule = (path: string) =>
  path
    .replace(/(^|.*?\/)(?=[A-Z])/, '')
    .replace(/\//g, '.')
    .replace(/\.hs$/, '')
