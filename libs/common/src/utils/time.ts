import { Slot, SlotConfig, SLOT_CONFIG_NETWORK as ORIGINAL_SLOT_CONFIG_NETWORK } from '@blaze-cardano/core'
import { Network } from './types'

export type UnixTime = number

export const SLOT_CONFIG_NETWORK: Record<Network, SlotConfig> = {
  ...ORIGINAL_SLOT_CONFIG_NETWORK, // Spread the original config
  Custom: { zeroTime: 0, zeroSlot: 0, slotLength: 1000 },
}

export function slotToBeginUnixTime(slot: Slot, slotConfig: SlotConfig): UnixTime {
  const msAfterBegin = (slot - slotConfig.zeroSlot) * slotConfig.slotLength
  return slotConfig.zeroTime + msAfterBegin
}

export function unixTimeToEnclosingSlot(unixTime: UnixTime, slotConfig: SlotConfig): Slot {
  const timePassed = unixTime - slotConfig.zeroTime
  const slotsPassed = slotConfig.slotLength >= 0 ? Math.floor(timePassed / slotConfig.slotLength) : 0

  return Slot(slotsPassed + slotConfig.zeroSlot)
}

export function unixTimeToSlot(unixTime: UnixTime, network: Network): Slot {
  return unixTimeToEnclosingSlot(unixTime, SLOT_CONFIG_NETWORK[network])
}

export function slotToUnixTime(slot: Slot, network: Network): UnixTime {
  return slotToBeginUnixTime(slot, SLOT_CONFIG_NETWORK[network])
}
