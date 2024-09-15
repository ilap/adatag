/// <reference types='vitest' />
import { defineConfig, PluginOption } from 'vite'
import react from '@vitejs/plugin-react'
import { nxViteTsPaths } from '@nx/vite/plugins/nx-tsconfig-paths.plugin'

import { nodePolyfills } from 'vite-plugin-node-polyfills'
import topLevelAwait from 'vite-plugin-top-level-await'
import wasm from 'vite-plugin-wasm'

export default defineConfig({
  root: __dirname,
  cacheDir: '../../node_modules/.vite/apps/adatag.io',

  server: {
    port: 4200,
    host: 'localhost',
    proxy: {
      '/local-cluster': {
        target: 'http://localhost:10000',
        changeOrigin: true,
        secure: false,
        headers: {
          // FIXME: for sqlite and proxy
          'Cross-Origin-Opener-Policy': 'same-site',
          'Cross-Origin-Embedder-Policy': 'require-corp',
        },
      },
    },
  },
  preview: {
    port: 4300,
    host: 'preview.adatag.io',
  },

  plugins: [
    wasm(),
    react(),
    nxViteTsPaths(),
    topLevelAwait(),
    nodePolyfills({
      globals: { Buffer: true, global: true },
      protocolImports: true,
    }),
  ],

  // Uncomment this if you are using workers.
  worker: {
    format: 'es',
    plugins: (): PluginOption[] => [wasm(), nxViteTsPaths()],
  },

  build: {
    outDir: '../../dist/apps/adatag.io',
    reportCompressedSize: true,
    commonjsOptions: {
      transformMixedEsModules: true,
    },
    cssCodeSplit: true,
    target: 'esnext',
    rollupOptions: {
      input: {
        main: 'index.html',
        worker: 'src/workers/TreeWorker.ts',
      },
      output: {
        manualChunks: {
          // Manually split code into smaller chunks
          vendor: [],
        },
      },
    },
  },
  test: {
    globals: true,
    cache: {
      dir: '../../node_modules/.vitest',
    },
    environment: 'jsdom',
    include: ['src/**/*.{test,spec}.{js,mjs,cjs,ts,mts,cts,jsx,tsx}'],

    reporters: ['default'],
    coverage: {
      reportsDirectory: '../../coverage/apps/adatag.io',
      provider: 'v8',
    },
  },

  optimizeDeps: {
    //include: ['uplc-node', , '@stricahq/cbors', '@meshsdk/react', 'isomorphic-ws'],
    exclude: ['@sqlite.org/sqlite-wasm', '@emurgo/cardano-message-signing-browser'],
  },
  resolve: {
    dedupe: ['buffer', 'Buffer'],
    alias: {
      ws: 'isomorphic-ws',
      'uplc-node': 'uplc-web',
    },
  },
})
