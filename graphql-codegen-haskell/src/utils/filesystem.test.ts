import * as fs from 'fs'

import { writeFile } from './filesystem'

jest.mock('fs')

const mockMkdirSync = fs.mkdirSync as jest.Mock
const mockWriteFileSync = fs.writeFileSync as jest.Mock

beforeEach(() => {
  mockMkdirSync.mockReset()
  mockWriteFileSync.mockReset()
})

describe('writeFile', () => {
  it('writes a file', () => {
    writeFile('foo.txt', 'Hello world!')

    expect(mockWriteFileSync).toHaveBeenCalledWith('foo.txt', 'Hello world!')
  })

  it('writes a file and creates parent directories', () => {
    writeFile('foo/bar/baz.txt', 'asdf')

    expect(mockMkdirSync).toHaveBeenCalledWith('foo/bar', { recursive: true })
    expect(mockWriteFileSync).toHaveBeenCalledWith('foo/bar/baz.txt', 'asdf')
  })

  it('ignores existing parent directories', () => {
    mockMkdirSync.mockImplementationOnce(() => {
      throw new Error('Directory already exists!')
    })

    writeFile('foo/bar/baz.txt', 'asdf')

    expect(mockWriteFileSync).toHaveBeenCalledWith('foo/bar/baz.txt', 'asdf')
  })
})
