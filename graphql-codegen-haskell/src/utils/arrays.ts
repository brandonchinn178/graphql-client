/**
 * Get rid of all nullish values in the given array.
 */
export const compact = <T>(arr: Array<T | null | undefined>): Array<T> =>
  arr.filter((v) => v !== null && v !== undefined) as Array<T>
