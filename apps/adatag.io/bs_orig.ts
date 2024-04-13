const BASE_PATH = '.'

console.log(`Bun runs at http://localhost:3000/index.html`)
Bun.serve({
  development: true,
  port: 3000,
  async fetch(req) {
    const filePath = BASE_PATH + new URL(req.url).pathname
    const file = Bun.file(filePath)
    return new Response(file)
  },
  error() {
    return new Response(null, { status: 404 })
  },
})
