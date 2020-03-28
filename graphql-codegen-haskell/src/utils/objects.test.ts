import { mergeObjects } from './objects'

describe('mergeObjects', () => {
  it('merges simple objects', () => {
    expect(mergeObjects([{ a: 1 }, { b: 2 }, { c: 3 }])).toEqual({
      a: 1,
      b: 2,
      c: 3,
    })
  })

  it('keeps keys in latter objects', () => {
    expect(mergeObjects([{ a: 1 }, { b: 2 }, { a: 3 }])).toEqual({
      a: 3,
      b: 2,
    })
  })
})
