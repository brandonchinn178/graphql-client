import * as yup from 'yup'

// See the "Configuration" section in the README for more details.
const CONFIG_SCHEMA = yup
  .object({
    schema: yup.string().required(),
    documents: yup.array().of(yup.string().defined()).ensure().required(),

    hsSourceDir: yup.string().default('src/').defined(),

    apiModule: yup.string().required(),
    enumsModule: yup.string().required(),
    scalarsModule: yup.string().required(),
  })
  .defined()

export type PluginConfig = {
  schema: string
  documents: string[]

  hsSourceDir: string

  apiModule: string
  enumsModule: string
  scalarsModule: string
}

export const validateConfig = (config: Record<string, unknown>): PluginConfig =>
  CONFIG_SCHEMA.validateSync(config)
