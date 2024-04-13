import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import wasm from 'vite-plugin-wasm'
import { NodeGlobalsPolyfillPlugin } from '@esbuild-plugins/node-globals-polyfill'
import { resolve } from 'path'

export default defineConfig({
  plugins: [wasm(), react()],
  optimizeDeps: {
    exclude: [
      '@sqlite.org/sqlite-wasm',
      '@emurgo/cardano-message-signing-browser',
      '@dcspark/cardano-multiplatform-lib-browser',
    ],
    esbuildOptions: {
      define: {
        global: 'globalThis',
      },
      plugins: [
        NodeGlobalsPolyfillPlugin({
          buffer: true,
        }),
      ],
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
  server: {
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
})
