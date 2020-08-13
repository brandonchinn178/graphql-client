/**
 * Loosely based on lodash's assign.
 */
export const mergeObjects = <T extends Record<string, unknown>>(
  objects: T[]
): T => objects.reduce((acc, obj) => ({ ...acc, ...obj }), {} as T)
