import React from 'react'

import { useConfig } from '../../hooks/useConfig'

const Chip = () => {
  const config = useConfig()
  return (
    <div className="bg-white border-black  border-1 text-foreground px-6 py-3 rounded-full fixed bottom-10 right-10">
      {config?.network}
    </div>
  )
}

export default Chip
