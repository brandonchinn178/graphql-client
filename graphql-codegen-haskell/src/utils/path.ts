/**
 * Convert the given file path into a Haskell module, e.g.
 *
 * pathToModule("src/Example/GraphQL/API.hs", "src/")
 * => "Example.GraphQL.API"
 *
 * Errors if the given source directory is not a prefix of the given path.
 */
export const pathToModule = (modulePath: string, srcDirRaw: string) => {
  const srcDir = ensureTrailingSlash(srcDirRaw)

  if (modulePath.indexOf(srcDir) !== 0) {
    throw new Error(
      `Path ${modulePath} does not start with directory ${srcDir}`
    )
  }

  return modulePath
    .substring(srcDir.length)
    .replace(/\//g, '.')
    .replace(/\.hs$/, '')
}

/**
 * Convert the given Haskell module into a file path, e.g.
 *
 * moduleToPath("Example.GraphQL.API", "src/")
 * => "src/Example/GraphQL/API.hs"
 */
export const moduleToPath = (moduleName: string, srcDir: string) => {
  const modulePath = moduleName.replace(/\./g, '/')
  return ensureTrailingSlash(srcDir) + modulePath + '.hs'
}

const ensureTrailingSlash = (path: string) =>
  path === '' ? path : path.replace(/\/*$/, '/')
