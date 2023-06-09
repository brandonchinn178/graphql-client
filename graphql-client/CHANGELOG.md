# Unreleased

* Add support for GHC 9.6

# v1.2.1

* Add support for GHC 9.4

# v1.2.0

Breaking changes:

* Remove support for GHC < 8.10

New features:

* Added `runQuerySafeIO` and expose `GraphQLManager` for applications that want to manually implement `MonadGraphQLQuery`

# v1.1.1

Bug fixes:

* Generate enums that only appear in query arguments ([#59](https://github.com/brandonchinn178/graphql-client/pull/59))

# v1.1.0

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

# v1.0.0

Initial release:

* Implement `graphql-client` Haskell library with a `graphql-codegen` executable that can generate Haskell definitions for `.graphql` files.
