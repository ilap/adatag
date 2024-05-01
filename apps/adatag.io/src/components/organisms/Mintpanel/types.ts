export type Rarity = {
  name: string
  color: ChipColor
  description: string
}

type ChipColor =
  | 'default'
  | 'primary'
  | 'secondary'
  | 'success'
  | 'warning'
  | 'danger'
  | undefined

export interface State {
  isModalOpen: boolean
  useAdahandle: boolean
}

export interface Action {
  type: string
}

export const initialState: State = {
  isModalOpen: false,
  useAdahandle: false,
}
