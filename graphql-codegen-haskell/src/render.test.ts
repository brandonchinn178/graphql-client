import { ParsedType } from './parse/variableDefinition'
import { renderHaskellType } from './render'

const NULLABLE = true

const graphqlScalar = (name: string, nullable = false): ParsedType => ({
  list: false,
  name,
  nullable,
})

const graphqlList = (inner: ParsedType, nullable = false): ParsedType => ({
  list: true,
  inner,
  nullable,
})

describe('render types for query arguments', () => {
  it.each`
    parsedType                                              | haskellType
    ${graphqlScalar('Int')}                                 | ${'Int'}
    ${graphqlScalar('Float')}                               | ${'Double'}
    ${graphqlScalar('String')}                              | ${'Text'}
    ${graphqlScalar('Boolean')}                             | ${'Bool'}
    ${graphqlScalar('ID')}                                  | ${'Text'}
    ${graphqlScalar('MyScalar')}                            | ${'MyScalar'}
    ${graphqlScalar('Int', NULLABLE)}                       | ${'Maybe Int'}
    ${graphqlList(graphqlScalar('Float'))}                  | ${'[Double]'}
    ${graphqlList(graphqlScalar('String'), NULLABLE)}       | ${'Maybe [Text]'}
    ${graphqlList(graphqlScalar('Boolean', NULLABLE))}      | ${'[Maybe Bool]'}
    ${graphqlList(graphqlScalar('ID', NULLABLE), NULLABLE)} | ${'Maybe [Maybe Text]'}
  `('renders `$haskellType`', ({ parsedType, haskellType }) => {
    expect(renderHaskellType(parsedType)).toBe(haskellType)
  })
})
