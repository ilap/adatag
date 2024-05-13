async function waitForOgmios(url: string, timeoutMs: number): Promise<boolean> {
  const startTime = Date.now()
  const endTime = startTime + timeoutMs

  while (Date.now() < endTime) {
    try {
      const response = await fetch(url)
      console.log(response.ok)
      return response.ok
    } catch (error) {
      console.log(`Still waiting...`)
    }

    await new Promise(resolve => setTimeout(resolve, 1000))
  }

  return false
}

const urlToCheck = Bun.argv[2]
const timeout = parseInt(Bun.argv[3]) * 1000

waitForOgmios(urlToCheck, timeout)
  .then(serverInfo => {
    if (serverInfo) {
      console.log(`Ogmios is up!`)
    } else {
      console.error(`Timeout: Server did not become available within ${timeout} ms.`)
      process.exit(1)
    }
  })
  .catch(error => {
    console.error(`Error: ${error.message}`)
    process.exit(1)
  })
