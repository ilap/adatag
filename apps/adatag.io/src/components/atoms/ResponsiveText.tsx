import React from 'react'

interface Props {
  text: string
}

export const ResponsiveText: React.FC<Props> = ({ text }) => {
  const textLength = text.length
  const fontSize = textLength <= 6 ? 'text-6xl' : textLength <= 10 ? 'text-4xl' : 'text-2xl'

  return (
    <div className={`font-bold  ${fontSize}`}>
      {text}
      <span className="bg-primary animate-blink">&nbsp;&nbsp;</span>
    </div>
  )
}
