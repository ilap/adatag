export const heroAnimation = {
  initial: {
    opacity: 0,
  },
  animate: {
    opacity: 1,
    transition: { duration: 1.5 },
  },
  exit: {
    opacity: 0,
    transition: { duration: 1.5 },
  },
}

export type PanelKey = 'default' | 'mint' | 'claim'

export type PanelState = {
  [key: string]: {
    title: JSX.Element
    subTitle: JSX.Element
    panel: JSX.Element | null
  }
}
