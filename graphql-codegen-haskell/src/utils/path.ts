/**
 * Convert the given Haskell module into a file path, e.g.
 *
 * moduleToPath("Example.GraphQL.API", "src/")
 * => "src/Example/GraphQL/API.hs"
 */
export const moduleToPath = (moduleName: string, srcDir: string): string => {
  const modulePath = moduleName.replace(/\./g, '/')
  return ensureTrailingSlash(srcDir) + modulePath + '.hs'
}

const ensureTrailingSlash = (path: string) =>
  path === '' ? path : path.replace(/\/*$/, '/')
