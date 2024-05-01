import React, { useState } from 'react'

interface DialogContextProps {
  isDialogOpen: boolean
  setIsDialogOpen: (isDialogOpen: boolean) => void
  toggleDialog: () => void
}

export const DialogContext = React.createContext<DialogContextProps>({
  isDialogOpen: false,
  setIsDialogOpen: () => {},
  toggleDialog: () => {},
})

export const DialogProvider = ({ children }: { children: React.ReactNode }) => {
  const [isDialogOpen, setIsDialogOpen] = useState(false)

  const toggleDialog = () => {
    setIsDialogOpen(!isDialogOpen)
  }

  return (
    <DialogContext.Provider
      value={{ isDialogOpen, setIsDialogOpen, toggleDialog }}
    >
      {children}
    </DialogContext.Provider>
  )
}
