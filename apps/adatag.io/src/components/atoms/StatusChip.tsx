import React from 'react'

import { useConfig } from '../../hooks/useConfig'

const Chip = () => {
  const config = useConfig()
  const bgColor = config?.network === 'mainnet' ? 'bg-black' : 'bg-red'
  const textColor = config?.network === 'mainnet' ? 'text-background' : 'text-foreground'

  return (
    <div
      className={`bg-black border-black text-foreground-50 border-1 px-6 py-4 rounded-full absolute bottom-10 right-10`}>
      {config?.network} Network
    </div>
  )
}

export default Chip
