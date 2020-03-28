/**
 * Loosely based on lodash's assign.
 */
export const mergeObjects = <T extends object>(objects: T[]): T =>
  objects.reduce((acc, obj) => ({ ...acc, ...obj }), {} as T)
