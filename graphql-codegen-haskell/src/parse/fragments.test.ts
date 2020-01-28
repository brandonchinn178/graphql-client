import gql from 'graphql-tag'

import { parseFragments } from './fragments'

it('parses fragments from AST', () => {
  const ast = gql`
    query getUser($name: String!) {
      getUser(name: $name) {
        ...userDetails
      }
    }

    fragment fooFragment on Foo {
      bar
    }

    mutation deleteUser($name: String!) {
      deleteUser(name: $name)
    }

    fragment userDetails on User {
      id
      name
    }
  `

  expect(parseFragments(ast)).toEqual({
    fooFragment: expect.objectContaining({
      name: expect.objectContaining({
        value: 'fooFragment',
      }),
    }),
    userDetails: expect.objectContaining({
      name: expect.objectContaining({
        value: 'userDetails',
      }),
    }),
  })
})
