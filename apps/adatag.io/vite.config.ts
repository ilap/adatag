import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react-swc'
import wasm from 'vite-plugin-wasm'
import { NodeGlobalsPolyfillPlugin } from '@esbuild-plugins/node-globals-polyfill'


export default defineConfig({
  build: {
    cssCodeSplit: true,
    target: "esnext",
  },
  plugins: [wasm(), react()],
  optimizeDeps: {
    esbuildOptions: {
      // Node.js global to browser globalThis
      define: {
        global: 'globalThis'
      },
      // Enable esbuild polyfill plugins
      plugins: [
        NodeGlobalsPolyfillPlugin({
          buffer: true
        })
      ]
    }
  }
})
