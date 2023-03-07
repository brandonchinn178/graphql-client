import { GraphQLFileLoader } from '@graphql-tools/graphql-file-loader'
import { JsonFileLoader } from '@graphql-tools/json-file-loader'
import * as graphqlTools from '@graphql-tools/load'
import { UrlLoader } from '@graphql-tools/url-loader'
import * as fs from 'fs'
import { concatAST, DocumentNode, GraphQLSchema } from 'graphql'
import * as yaml from 'js-yaml'
import Mustache from 'mustache'
import * as path from 'path'

import { validateConfig } from './config'
import { parseFragments } from './parse/fragments'
import { ParsedEnum, parseOperations } from './parse/operation'
import { renderAPIModule } from './render/api'
import { renderEnumModule } from './render/enum'
import { compact, moduleToPath, writeFile } from './utils'

export const main = (): void => {
  const error = (msg: string) => {
    console.error(msg)
    process.exit(1)
  }

  const argv = process.argv.slice(2)

  if (argv.length !== 1) {
    error(`Usage: ${process.argv[1]} CONFIG`)
  }

  const [configPath] = argv

  generate(configPath)
    .then((outputFiles) => {
      Object.entries(outputFiles).forEach(([modulePath, content]) => {
        writeFile(modulePath, content)
      })
    })
    .catch(error)
}

type OutputFiles = {
  [modulePath: string]: string
}

export const generate = async (configPath: string): Promise<OutputFiles> => {
  // load configuration

  const rootDir = path.dirname(configPath)
  const rawConfig = yaml.safeLoad(fs.readFileSync(configPath, 'utf-8'))
  const config = validateConfig(rawConfig)

  // load schema + documents

  const schema = await loadSchema(config.schema, rootDir)
  const ast = await loadDocuments(config.documents, rootDir)

  // parse schema + documents

  const parsedFragments = parseFragments(ast)
  const { enums, operations } = parseOperations(ast, schema, parsedFragments)

  // render modules

  const filesToGenerate: Record<string, string> = {}

  const toEnumModule = (parsedEnum: ParsedEnum) =>
    `${config.enumsModule}.${parsedEnum.name}`

  const addModule = (module: string, content: string) => {
    const sourceDir = path.resolve(rootDir, config.hsSourceDir)
    const modulePath = moduleToPath(module, sourceDir)
    filesToGenerate[modulePath] = content
  }

  // globally disable html-escaping
  Mustache.escape = x => x

  addModule(
    config.apiModule,
    renderAPIModule(config, enums.map(toEnumModule), operations)
  )

  enums.forEach((parsedEnum) => {
    const enumModule = toEnumModule(parsedEnum)
    addModule(enumModule, renderEnumModule(config, parsedEnum, enumModule))
  })

  return filesToGenerate
}

const loadSchema = async (
  schema: string,
  rootDir: string
): Promise<GraphQLSchema> => {
  const schemaAst = await graphqlTools.loadSchema(schema, {
    assumeValidSDL: true,
    loaders: [new GraphQLFileLoader(), new JsonFileLoader(), new UrlLoader()],
    sort: true,
    convertExtensions: true,
    cwd: rootDir,
  })

  return schemaAst
}

const loadDocuments = async (
  documentPaths: string[],
  rootDir: string
): Promise<DocumentNode> => {
  const documents = await graphqlTools.loadDocuments(documentPaths, {
    loaders: [new GraphQLFileLoader()],
    sort: true,
    skipGraphQLImport: true,
    cwd: rootDir,
  })

  return concatAST(compact(documents.map((v) => v.document)))
}

if (require.main === module) {
  main()
}
