import { useState, useEffect } from 'react'
import { GenesisConfig } from '../utils/types'
import { getConfig } from '../utils/config'

export function useConfig() {
  const [config, setConfig] = useState<GenesisConfig | undefined>(undefined)

  useEffect(() => {
    ;(async function fetchConfig() {
      try {
        const response = await await getConfig()
        setConfig(response)
      } catch (e) {
        console.log(e)
      }
    })()
  }, [])

  return config
}
