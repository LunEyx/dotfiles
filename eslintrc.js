module.exports = {
  'env': {
    'browser': true,
    'commonjs': true,
    'es6': true
  },
  'extends': [
    'eslint:recommended',
    'plugin:react/recommended',
  ],
  'parserOptions': {
    'ecmaFeatures': {
      'jsx': true
    },
    'ecmaVersion': 2018,
    'sourceType': 'module'
  },
  'plugins': [
    'react'
  ],
  'rules': {
    'no-unused-vars': [ 'warn' ],
    'indent': [ 'error', 2, { 'SwitchCase': 1 } ],
    'linebreak-style': [ 'error', 'unix' ],
    'quotes': [ 'error', 'single' ],
    'react/jsx-filename-extension': [1, {'extensions': ['.js', '.jsx'] }],
    'semi': [ 'error', 'always' ],
    'react/prop-types': [0],
    'jsx-a11y/anchor-is-valid' : [0],
  }
};
