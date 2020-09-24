## Upcoming

Breaking changes:

* Require `aeson-schemas-1.3.0`
    * `TypeApplications` is no longer needed for `get` quasiquoters
    * See `aeson-schemas` CHANGELOG for more details
* Scalars now also need a `ToJSON` instance

Miscellaneous changes:

* Improved test-utils UX:
    * Export `AnyResultMock`
    * Add `Show` instance for `AnyResultMock`
    * Add `MonadTrans` instance for `MockQueryT`

## 1.0.0

Initial release:

* Implement `graphql-client` Haskell library with a `graphql-codegen` executable that can generate Haskell definitions for `.graphql` files.
