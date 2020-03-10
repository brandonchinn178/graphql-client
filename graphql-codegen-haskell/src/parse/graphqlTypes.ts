interface BaseParsedType<IsList extends boolean> {
  list: IsList
  nullable: boolean
}

export interface ParsedScalarType extends BaseParsedType<false> {
  name: string
}

export interface ParsedListType<T> extends BaseParsedType<true> {
  inner: T
}

export interface ParsedObjectType<T> extends BaseParsedType<false> {
  fields: T
}

export interface ParsedUnionType<T> extends BaseParsedType<false> {
  subTypes: T[]
}

/** Helpers **/

export const NULLABLE = true

export const graphqlScalar = (
  name: string,
  nullable = false
): ParsedScalarType => ({
  list: false,
  name,
  nullable,
})

export const graphqlList = <T>(
  inner: T,
  nullable = false
): ParsedListType<T> => ({
  list: true,
  inner,
  nullable,
})

export const graphqlObject = <T>(
  fields: T,
  nullable = false
): ParsedObjectType<T> => ({
  list: false,
  fields,
  nullable,
})

export const graphqlUnion = <T>(subTypes: T[]): ParsedUnionType<T> => ({
  list: false,
  subTypes,
  nullable: false,
})
