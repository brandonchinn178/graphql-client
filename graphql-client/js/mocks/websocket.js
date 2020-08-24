/**
 * websocket has native bindings, which rollup can't bundle, so we're mocking
 * out. The library is only used by @graphql-tools/url-loader if subscriptions
 * are enabled, which we're not using, so this mock shouldn't be called at all.
 */

require('./__utils').exportMockedModule('websocket')
