const ResponsiveText = ({ text }: { text: string }) => {
  const textLength = text.length
  let fontSize: string

  if (textLength <= 6) {
    fontSize = 'text-6xl'
  } else if (textLength <= 10) {
    fontSize = 'text-4xl'
  } else {
    fontSize = 'text-2xl'
  }

  return (
    <div className={`font-bold  ${fontSize}`}>
      {text}
      <span className="animate-blink">&nbsp;&nbsp;</span>
    </div>
  )
}

export default ResponsiveText
