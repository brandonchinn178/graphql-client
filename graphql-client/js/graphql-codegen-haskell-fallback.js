/**
 * This file should be loaded if, for some reason, `graphql-codegen-haskell.js`
 * isn't built yet.
 *
 * If this is the case, building `graphql-client` SHOULD NOT fail, but running
 * `graphql-codegen` SHOULD fail UNLESS the user gets a copy of
 * `graphql-codegen-haskell.js` themselves and points to it via the
 * `GRAPHQL_CODEGEN` environment variable.
 */

const fs = require('fs')
const path = require('path')
const utils = require('__utils')

const ERROR_MESSAGE = `
The graphql-codegen executable was built without the
graphql-codegen-haskell.js bundle. This can happen if you're building the
graphql-client package from a source other than Hackage.

If this is the case, get the graphql-codegen-haskell.js bundle manually and set
the GRAPHQL_CODEGEN environment variable to the location of that bundle. You
can get the bundle in one of the following ways:

  1. Download from CI (recommended)
      a. Go to ${utils.GITHUB_URL}
      b. Navigate to the appropriate commit
      c. Click on the commit status icon and click on the any of the CI links
      d. Click on the "Summary" tab
      e. Under "Artifacts", download 'graphql-codegen-haskell.js'
  2. Build manually
      a. Clone ${utils.GITHUB_URL}
      b. Follow the instructions in DEVELOPER.md to build the bundle
`

const error = (msg) => {
  console.error(['*'.repeat(80), msg, ERROR_MESSAGE, '*'.repeat(80)].join('\n'))
  process.exit(1)
}

const requireFile = (file) => {
  try {
    return require(file)
  } catch (e) {
    error(`Could not load file ${file}:\n${e}`)
  }
}

const GRAPHQL_CODEGEN = process.env.GRAPHQL_CODEGEN
if (GRAPHQL_CODEGEN === undefined) {
  error('GRAPHQL_CODEGEN environment variable not set')
}

const codegenFileSrc = path.resolve(process.cwd(), GRAPHQL_CODEGEN)
const codegenFile = path.resolve(
  __dirname,
  './graphql-codegen-haskell-external.js'
)

// require(...) searches for a node_modules directory in the parents of the JS
// file being required, so copy the file into here.
fs.copyFileSync(codegenFileSrc, codegenFile)

requireFile(codegenFile).main()
