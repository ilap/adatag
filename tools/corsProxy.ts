import express from 'express'
import cors from 'cors'
import { createProxyMiddleware } from 'http-proxy-middleware'

const PORT = 3000
const app = express()

app.use(cors())

const exampleProxy = createProxyMiddleware({
  target: 'http://localhost:10000',
  changeOrigin: true,
})

app.use('/', exampleProxy)

app.listen(PORT, () => {
  console.log(`CORS Proxy server is running on http://localhost:${PORT}`)
})
