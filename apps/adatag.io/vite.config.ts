import { defineConfig } from 'vite'
import { createHtmlPlugin } from 'vite-plugin-html'
import { resolve } from 'path'

import react from '@vitejs/plugin-react-swc'
import wasm from 'vite-plugin-wasm'
import { NodeGlobalsPolyfillPlugin } from '@esbuild-plugins/node-globals-polyfill'
import typescript from '@rollup/plugin-typescript';

export default defineConfig({
  rollupOptions: {
    external: [
      '@emurgo/cardano-message-signing-nodejs',
      '@emurgo/cardano-serialization-lib-nodejs',
      'axios', 'bip39', 'nanoid', 'zod',
    ],
    plugins: [
      typescript(),
    ]
  },
  build: {
    cssCodeSplit: true,
    target: 'esnext',
    rollupOptions: {
      input: {
        main: 'index.html',
        worker: 'src/workers/TreeWorker.ts',
      },
    },
  },
  plugins: [
    wasm(),
    react(),
    createHtmlPlugin({
      inject: {
        data: {
          injectScript: `<script src="coi-serviceworker.min.js"></script>`,
        },
      },
    }),
  ],
  worker: {
    format: 'es',
    plugins: [wasm()],
  },
  optimizeDeps: {
    exclude: [
      '@sqlite.org/sqlite-wasm',
      '@emurgo/cardano-message-signing-browser',
      '@dcspark/cardano-multiplatform-lib-browser',
    ],
    esbuildOptions: {
      // Node.js global to browser globalThis
      define: {
        global: 'globalThis',
      },
      // Enable esbuild polyfill plugins
      plugins: [
        NodeGlobalsPolyfillPlugin({
          buffer: true,
        }),
      ],
    },
  },
  server: {
    headers: {
      //'Cross-Origin-Opener-Policy': 'same-origin',
      //'Cross-Origin-Resource-Policy': 'cross-origin',
      //'Cross-Origin-Embedder-Policy': 'credentialless'
      // COOP COEP
      'Cross-Origin-Embedder-Policy': 'require-corp',
      'Cross-Origin-Opener-Policy': 'same-origin',
    },
    cors: {
      origin: '*', // Allow requests from any origin
    },
    proxy: {
      '/local-cluster': {
        target: 'http://localhost:10000',
        changeOrigin: true,
        secure: false,
      },
    },
  },
  resolve: {
    alias: {
      '@adatag/shared/config': resolve(
        __dirname,
        '../../libs/deployment-ts/src/config/',
      ),
      '@adatag/integri-tree': resolve(
        __dirname,
        '../../libs/integri-tree/src/',
      ),
      '@adatag/shared/plutus': resolve(
        __dirname,
        '../../libs/deployment-ts/src/plutus/',
      ),
    },
  },
})
