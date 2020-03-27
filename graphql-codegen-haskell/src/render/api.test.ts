import {
  graphqlList,
  graphqlObject,
  graphqlScalar,
  graphqlUnion,
  NULLABLE,
} from '~/parse/graphqlTypes'
import { ParsedSelection } from '~/parse/selectionSet'

import { renderAesonSchema, renderHaskellType } from './api'

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

it('renders a schema as appropriate for aeson-schemas', () => {
  const selections = {
    // scalars
    int: graphqlScalar('Int'),
    float: graphqlScalar('Float'),
    string: graphqlScalar('String'),
    bool: graphqlScalar('Boolean'),
    id: graphqlScalar('ID'),
    custom: graphqlScalar('FooScalar'),

    // lists
    list: graphqlList(graphqlScalar('Float')),
    listNull: graphqlList(graphqlScalar('Boolean', NULLABLE)),
    listObject: graphqlList(
      graphqlObject({
        int: graphqlScalar('Int'),
      })
    ),
    listNullObject: graphqlList(
      graphqlObject(
        {
          int: graphqlScalar('Int'),
        },
        NULLABLE
      )
    ),

    // objects
    object: graphqlObject({
      id: graphqlScalar('ID'),
      nullInt: graphqlScalar('Int', NULLABLE),
    }),

    // unions
    union: graphqlUnion<ParsedSelection>([
      {
        id: graphqlScalar('ID'),
        name: graphqlScalar('String'),
      },
      { a: graphqlScalar('Int') },
      { list: graphqlList(graphqlScalar('Bool'), NULLABLE) },
    ]),

    // nullable
    nullInt: graphqlScalar('Int', NULLABLE),
    nullCustom: graphqlScalar('FooScalar', NULLABLE),
    nullList: graphqlList(graphqlScalar('String'), NULLABLE),
    nullListNull: graphqlList(graphqlScalar('ID', NULLABLE), NULLABLE),
    nullListObject: graphqlList(
      graphqlObject({
        int: graphqlScalar('Int'),
      }),
      NULLABLE
    ),
    nullListNullObject: graphqlList(
      graphqlObject(
        {
          int: graphqlScalar('Int'),
        },
        NULLABLE
      ),
      NULLABLE
    ),
    nullObject: graphqlObject(
      {
        int: graphqlScalar('Int'),
        list: graphqlList(graphqlScalar('Float')),
        custom: graphqlScalar('FooScalar'),
      },
      NULLABLE
    ),
  }

  expect(renderAesonSchema(selections)).toMatchInlineSnapshot(`
    "{
      int: Int,
      float: Double,
      string: Text,
      bool: Bool,
      id: Text,
      custom: FooScalar,
      list: List Double,
      listNull: List Maybe Bool,
      listObject: List {
        int: Int,
      },
      listNullObject: List Maybe {
        int: Int,
      },
      object: {
        id: Text,
        nullInt: Maybe Int,
      },
      union: {
        id: Text,
        name: Text,
      } | {
        a: Int,
      } | {
        list: Maybe List Bool,
      },
      nullInt: Maybe Int,
      nullCustom: Maybe FooScalar,
      nullList: Maybe List Text,
      nullListNull: Maybe List Maybe Text,
      nullListObject: Maybe List {
        int: Int,
      },
      nullListNullObject: Maybe List Maybe {
        int: Int,
      },
      nullObject: Maybe {
        int: Int,
        list: List Double,
        custom: FooScalar,
      },
    }"
  `)
})
