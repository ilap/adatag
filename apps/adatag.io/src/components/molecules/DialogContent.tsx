import React, { ReactNode } from 'react'
import { Image } from '@nextui-org/react'

interface Props {
  icon: string
  title: string
  subtitle: ReactNode
}

export const DialogContent: React.FC<Props> = ({ icon, title, subtitle }) => {
  return (
    <div className="flex flex-col items-center">
      <div className={`text-6xl mb-4`}>
        <Image src={icon} alt="Dialog icon" width={200} height={200} />
      </div>
      <div className="text-3xl font-medium  tracking-tighter mb-2">{title}</div>
      <div className="text-sm mb-4">{subtitle}</div>
    </div>
  )
}
