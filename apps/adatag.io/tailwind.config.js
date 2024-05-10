/** @type {import('tailwindcss').Config} */

const {nextui} = require("@nextui-org/react");

export default {
  content: [
  './index.html', 
  './src/**/*.{js,ts,jsx,tsx}',
  '../../node_modules/@nextui-org/theme/dist/**/*.{js,ts,jsx,tsx}'
],
  theme: {
    extend: {
      colors: {
        accent: {
          1: 'var(--accent1)',
        },
        primary: 'var(--at-primary)',
        onPrimary: 'var(--at-on-primary)',
        secondary: 'var(--at-secondary)',
        onSecondary: 'var(--at-on-secondary)',
        backGround: 'var(--at-background)',
        onBackground: 'var(--at-on-background)',
      },
    },
  },
  darkMode: "class",
  plugins: [nextui()],
}
