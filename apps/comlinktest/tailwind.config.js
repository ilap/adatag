/** @type {import('tailwindcss').Config} */
export default {
  content: ["./index.html", "./src/**/*.{js,ts,jsx,tsx}"],
  theme: {
    extend: {
      colors: {
        accent: {
          1: "var(--accent1)",
        },
        primary: "var(--at-primary)",
        onPrimary: "var(--at-on-primary)",
        secondary: "var(--at-secondary)",
        onSecondary: "var(--at-on-secondary)",
        backGround: "var(--at-background)",
        onBackground: "var(--at-on-background)",
      },
    },
  },
  plugins: [],
};
