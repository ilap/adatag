const { FlatCompat } = require('@eslint/eslintrc')
const nxEslintPlugin = require('@nx/eslint-plugin')
const typescriptEslintEslintPlugin = require('@typescript-eslint/eslint-plugin')
const typescriptEslintParser = require('@typescript-eslint/parser')
const js = require('@eslint/js')

const compat = new FlatCompat({
  baseDirectory: __dirname,
  recommendedConfig: js.configs.recommended,
})

module.exports = [
  {
    plugins: {
      '@nx': nxEslintPlugin,
      '@typescript-eslint': typescriptEslintEslintPlugin,
    },
  },
  { languageOptions: { parser: typescriptEslintParser } },
  {
    files: ['**/*.ts', '**/*.tsx', '**/*.js', '**/*.jsx'],
    rules: {
      '@nx/enforce-module-boundaries': [
        'error',
        {
          enforceBuildableLibDependency: true,
          allow: [],
          depConstraints: [
            {
              sourceTag: '*',
              onlyDependOnLibsWithTags: ['*'],
            },
          ],
        },
      ],
    },
  },
  ...compat
    .config({
      extends: [
        'plugin:@nx/typescript',
        'eslint:recommended',
        'plugin:json/recommended',
        'plugin:@typescript-eslint/recommended',
        'plugin:@typescript-eslint/recommended-type-checked',
      ],
    })
    .map(config => ({
      ...config,
      files: ['**/*.ts', '**/*.tsx'],
      rules: {},
    })),
  { ignores: ['contracts'] },
]
