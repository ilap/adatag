const Status = ({ connected }: { connected: boolean }) => {
    const backgroundColor = connected ? '#00FF00' : '#FF0000'
    const textColor = connected ? '#000000' : '#FFFFFF'
    const text = connected ? 'Connected' : 'Disconnected'
  
    return (
      <>
        <div className="absolute bottom-10 left-10">
          <div
            className="rounded-full px-4 py-2 text-sm font-medium"
            style={{ backgroundColor, color: textColor }}
          >
            {text}
          </div>
          <div>
            <span className="relative flex h-2 w-2">
              <span className=" bg-orange-900 absolute -top-1 -left-1 inline-flex h-4 w-4 animate-ping rounded-full" />
              {/*<span className='bg-black relative inline-flex h-2 w-2 rounded-full'/>*/}
            </span>
          </div>
          <p>alma</p>
        </div>
      </>
    )
  }