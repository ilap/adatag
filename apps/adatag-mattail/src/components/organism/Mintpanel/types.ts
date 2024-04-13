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
