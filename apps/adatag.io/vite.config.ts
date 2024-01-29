/// <reference types='vitest' />
import { defineConfig, type UserConfig } from "vite";
import { qwikVite } from "@builder.io/qwik/optimizer";
import { qwikCity } from "@builder.io/qwik-city/vite";
import tsconfigPaths from "vite-tsconfig-paths";
import { nxViteTsPaths } from '@nx/vite/plugins/nx-tsconfig-paths.plugin';

export default defineConfig({
  root: __dirname,

  build: {
    outDir: '../../dist/apps/adatag.io',
    reportCompressedSize: true,
    commonjsOptions: {
      transformMixedEsModules: true,
    },
  },
  cacheDir: '../../node_modules/.vite/adatag.io',
  server: {
    port: 4200,
    host: 'localhost',
    headers: {
      "Cache-Control": "public, max-age=0",
    },
    fs: {
      // Allow serving files from the project root
      allow: ['../../'],
    },
  },

  preview: {
    port: 4300,
    host: 'localhost',
    headers: {
      "Cache-Control": "public, max-age=600",
    },
  },

  plugins: [qwikCity(), qwikVite({
    client: {
      outDir: '../../dist/apps/adatag.io',
    },
    ssr: {
      outDir: '../../dist/apps/adatag.io',
    },
    tsconfigFileNames: ['tsconfig.app.json'],
  }),
  // BUG: vite does not compile if it has this: tsconfigPaths({ root: '../../' }),
    , nxViteTsPaths(),],

  test: {
    reporters: ['default'],
    coverage: {
      reportsDirectory: '../../coverage/apps/adatag.io',
      provider: 'v8',
    },
    globals: true,
    cache: {
      dir: '../../node_modules/.vitest',
    },
    environment: 'jsdom',
    include: ['src/**/*.{test,spec}.{js,mjs,cjs,ts,mts,cts,jsx,tsx}'],
  },
});
