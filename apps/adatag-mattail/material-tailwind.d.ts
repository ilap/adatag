import '@material-tailwind/react'

declare module '@material-tailwind/react' {
  interface ButtonProps {
    placeholder?: string | undefined
    onPointerEnterCapture?:
      | React.PointerEventHandler<HTMLButtonElement>
      | undefined
    onPointerLeaveCapture?:
      | React.PointerEventHandler<HTMLButtonElement>
      | undefined
  }

  interface InputProps {
    crossOrigin?: any
  }
}
