/* global Proxy */

const ISSUE_PATH = 'https://github.com/LeapYear/graphql-client/issues/'

module.exports = {
  /**
   * Export an object for this module that will error if any export tries
   * to be accessed.
   */
  exportMockedModule: (name) => {
    module.exports = new Proxy(
      {},
      {
        get: function () {
          throw new Error(
            [
              `Attempted to import the module: ${name}`,
              'Something went very, very wrong.',
              `Please report this to ${ISSUE_PATH}`,
            ].join('\n')
          )
        },
      }
    )
  },
}
