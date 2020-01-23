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
