import * as fs from 'fs'
import * as path from 'path'

/**
 * Synchronously write a file to the given path, creating directories if
 * necessary.
 *
 * Usage:
 *
 *   writeFile('foo/bar/asdf.txt', 'contents')
 */
export const writeFile = (filepath: string, content: string): void => {
  try {
    fs.mkdirSync(path.dirname(filepath), { recursive: true })
  } catch (_) {}

  fs.writeFileSync(filepath, content)
}
