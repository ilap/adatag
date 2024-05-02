const cors_proxy = require('cors-anywhere')

// Define the port for the proxy server
const PORT = 3000

// Start the proxy server
cors_proxy
  .createServer({
    originWhitelist: [], // Allow all origins
  })
  .listen(PORT, () => {
    console.log(`CORS Anywhere is running on http://localhost:${PORT}`)
  })
