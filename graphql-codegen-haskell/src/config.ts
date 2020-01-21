import * as yup from 'yup'

const CONFIG_SCHEMA = yup.object({
  // The Haskell module being written. By default will parse the output file
  // path
  apiModule: yup.string(),

  // The Haskell module containing all the scalar definitions
  scalarsModule: yup.string().required(),
})

export type PluginConfig = yup.InferType<typeof CONFIG_SCHEMA>

export const validateConfig = (config: { [key: string]: unknown }) => {
  CONFIG_SCHEMA.validateSync(config)
}
