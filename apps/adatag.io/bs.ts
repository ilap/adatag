const BASE_PATH = '.'
const PORT = 3010
console.log(`Bun runs at http://localhost:${PORT}/index.html`)

Bun.serve({
  development: true,
  port: PORT,
  async fetch(req) {
    const filePath = BASE_PATH + new URL(req.url).pathname
    const file = Bun.file(filePath)

    try {
      const contentType = (await file.text()).endsWith('.json')
        ? 'application/javascript'
        : ''

      if (contentType === '') {
        return new Response(file)
      } else {
        const fileContent = await file.text()
        return new Response(file, { headers: { 'Content-Type': contentType } })
      }
    } catch (error) {
      console.error(error)
      return new Response(null, { status: 500 })
    }
  },
  error() {
    return new Response(null, { status: 404 })
  },
})
