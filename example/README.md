# GraphQL Client Example

An example usage of the `graphql-client` library that hits the
`https://graphbrainz.herokuapp.com` GraphQL API. The `.graphql` files
can be found at `src/Example/GraphQL/api/`, with the generated files
found at `src/Example/GraphQL/API.hs` and `src/Example/GraphQL/Enums/`.

## Usage

```bash
stack build
stack exec graphql-client-example
```

## Test

```bash
stack test
```

## Regenerate GraphQL

```bash
stack build :graphql-codegen
example/scripts/codegen.sh
```
