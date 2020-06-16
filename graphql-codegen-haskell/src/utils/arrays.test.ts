import { compact } from './arrays'

describe('compact', () => {
  it('Filters out nulls', () => {
    expect(compact([1, null, 2, 3, null])).toStrictEqual([1, 2, 3])
  })

  it('Filters out undefineds', () => {
    expect(compact([1, undefined, 2, 3, undefined])).toStrictEqual([1, 2, 3])
  })
})
