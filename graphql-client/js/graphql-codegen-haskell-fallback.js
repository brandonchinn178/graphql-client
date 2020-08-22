/**
 * This file should be loaded if, for some reason, `graphql-codegen-haskell.js`
 * isn't built yet.
 *
 * If this is the case, building `graphql-client` SHOULD NOT fail, but running
 * `graphql-codegen` SHOULD fail UNLESS the user builds
 * `graphql-codegen-haskell.js` themselves and points to it via the
 * `GRAPHQL_CODEGEN` environment variable.
 */

const path = require('path')
const utils = require('__utils')

const ERROR_MESSAGE = `
The graphql-codegen executable was built without the
graphql-codegen-haskell.js bundle. This can happen if you're building
the graphql-client package from a source other than Hackage.

If this is the case, please clone the GitHub repo found at:

  ${utils.GITHUB_URL}

and follow the instructions in DEVELOPER.md to build the
graphql-codegen-haskell.js bundle manually. Then set the GRAPHQL_CODEGEN
environment variable to the location of that bundle.
`

const error = (msg) => {
  console.error(['*'.repeat(80), msg, ERROR_MESSAGE, '*'.repeat(80)].join('\n'))
  process.exit(1)
}

const GRAPHQL_CODEGEN = process.env.GRAPHQL_CODEGEN
if (GRAPHQL_CODEGEN === undefined) {
  error('GRAPHQL_CODEGEN environment variable not set')
}

const codegenFile = path.resolve(process.cwd(), GRAPHQL_CODEGEN)
let codegen

try {
  codegen = require(codegenFile)
} catch (_) {
  error(`File not found: ${codegenFile}`)
}

codegen.main()
