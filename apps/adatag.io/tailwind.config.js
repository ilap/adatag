const { createGlobPatternsForDependencies } = require('@nx/react/tailwind')
const { join } = require('path')
const { nextui } = require('@nextui-org/react')

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    './index.html',
    './src/**/*.{vue,js,ts,jsx,tsx}',
    '../../node_modules/@nextui-org/theme/dist/**/*.{js,ts,jsx,tsx}',
  ],
  darkMode: 'class',
  plugins: [
    nextui({
      layout: {},
      themes: {
        light: {
          colors: {
            primary: {
              DEFAULT: '#ffbf04',
              foreground: '#000000',
            },
            focus: '#ffbf04',
          },
        },
      },
    }),
  ],
  theme: {
    extend: {
      fontSize: {
        '9xl': '7rem',
        '10xl': '8rem',
      },
      borderRadius: {
        xl: '2rem',
      },
      animation: {
        blink: 'blink 1250ms step-end infinite',
      },
      keyframes: {
        blink: {
          'from, to': {
            opacity: '100%',
          },
          '50%': {
            opacity: '0%',
          },
        },
      },
    },
  },
}
