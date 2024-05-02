const cors_proxy = require('cors-anywhere')

const PORT = 3000

cors_proxy
  .createServer({
    originWhitelist: [], // Allow all origins
  })
  .listen(PORT, () => {
    console.log(`CORS Anywhere is running on http://localhost:${PORT}`)
  })
