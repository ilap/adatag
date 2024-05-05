interface ServerInfo {
  nodePort: number
  submitApiPort: number
  socketPath: string
  protocolMagic: number
  slotLength: number
  blockTime: number
  epochLength: number
  p2pEnabled: boolean
  startTime: number
  masterNode: boolean
  adminNodeUrl: string | null
  ogmiosPort: number
  kupoPort: number
  yaciStorePort: number
  socatPort: number
  blockProducer: boolean
}

async function waitForServer(port: number, timeoutMs: number): Promise<ServerInfo | null> {
  const startTime = Date.now()
  const endTime = startTime + timeoutMs

  while (Date.now() < endTime) {
    try {
      const response = await fetch(`http://localhost:${port}/local-cluster/api/admin/clusters/default`)

      if (response.ok) {
        const serverInfo: ServerInfo = await response.json()
        if (serverInfo.startTime !== 0) {
          return serverInfo
        }
      }
    } catch (error) {
      console.log(`Still waiting...`)
    }

    await new Promise(resolve => setTimeout(resolve, 1000))
  }

  return null
}

const portToCheck = parseInt(Bun.argv[2])
const timeoutMilliseconds = parseInt(Bun.argv[3]) * 1000

waitForServer(portToCheck, timeoutMilliseconds)
  .then(serverInfo => {
    if (serverInfo) {
      console.log(`Server is up! Start Time: ${serverInfo.startTime}`)
    } else {
      console.error(`Timeout: Server did not become available within ${timeoutMilliseconds} ms.`)
      process.exit(1)
    }
  })
  .catch(error => {
    console.error(`Error: ${error.message}`)
    process.exit(1)
  })
