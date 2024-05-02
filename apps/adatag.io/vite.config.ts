/// <reference types='vitest' />
import { defineConfig, PluginOption } from 'vite'
import react from '@vitejs/plugin-react'
import wasm from 'vite-plugin-wasm'
import { NodeGlobalsPolyfillPlugin } from '@esbuild-plugins/node-globals-polyfill'
import { nxViteTsPaths } from '@nx/vite/plugins/nx-tsconfig-paths.plugin'

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
          'Access-Control-Allow-Origin': '*',
          'Access-Control-Allow-Methods': 'GET, POST, PUT, DELETE, OPTIONS',
          'Access-Control-Allow-Headers': 'Content-Type, Authorization',
        },
      },
    },
  },
  preview: {
    port: 4300,
    host: 'localhost',
  },

  plugins: [wasm(), react(), nxViteTsPaths()],

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
    exclude: [
      '@sqlite.org/sqlite-wasm',
      '@emurgo/cardano-message-signing-browser',
      '@dcspark/cardano-multiplatform-lib-browser',
    ],
    esbuildOptions: {
      define: {
        global: 'globalThis',
        process: 'process',
        Buffer: 'Buffer',
      },
      plugins: [
        NodeGlobalsPolyfillPlugin({
          buffer: true,
        }),
      ],
    },
  },
  resolve: {
    dedupe: ['buffer', 'Buffer'],
  },
})
