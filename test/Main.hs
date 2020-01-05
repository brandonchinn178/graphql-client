import Test.Tasty (defaultMain, testGroup)

import Data.GraphQL.Test.Monad.Class

main :: IO ()
main = defaultMain $ testGroup "graphql-client"
  [ testRunQuery
  ]
