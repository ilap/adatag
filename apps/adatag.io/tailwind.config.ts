import type { Config } from 'tailwindcss'

import { nextui } from '@nextui-org/react'

export default {
  content: [
    './index.html',
    './src/**/*.{vue,js,ts,jsx,tsx}',
    '../../node_modules/@nextui-org/theme/dist/**/*.{js,ts,jsx,tsx}',
  ],
  darkMode: 'class',
  plugins: [nextui({
    themes: {
      light: {
        colors: {
          primary: {
            DEFAULT: "#ffbf04",
            foreground: "#000000",
          },
          focus: "#ffbf04",
        },
      },
    },
  }),],
} satisfies Config
