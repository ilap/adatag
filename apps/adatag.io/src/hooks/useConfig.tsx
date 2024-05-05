import { useState, useEffect } from 'react'
import { GenesisConfig } from '../utils/types'
import { genesisConfig } from '../utils/config'

export function useConfig() {
  const [config, setConfig] = useState<GenesisConfig | undefined>(undefined)

  useEffect(() => {
    (async function fetchConfig() {
      try {
        setConfig(genesisConfig)
      } catch (e) {
        console.log(e)
      }
    })()
  }, [])

  return config
}
