import { SearchState } from '../../../hooks/useDebouncedSearch'
import {
  infoCaption,
  errorCaption,
  invalidAdatag,
  notAvailableCaption,
  availableCaption,
  redeemAvailableCaption,
  redeemNotAvailableCaption,
} from './constants'
import { Rarity } from './types'

export const getCaption = (state: SearchState, minting: boolean = true): string => {
  switch (state) {
    case SearchState.Error:
      return errorCaption
    case SearchState.Initial:
      return infoCaption
    case SearchState.InvalidAdatag:
      return invalidAdatag
    case SearchState.Minted:
      return minting ? notAvailableCaption : redeemNotAvailableCaption
    case SearchState.NotMinted:
      return minting ? availableCaption : redeemAvailableCaption
    default:
      return infoCaption
  }
}

export const getRarity = (length: number): Rarity => {
  if (length === 1)
    return {
      name: 'Legendary',
      color: 'danger',
      description: 'Extremely rare and valuable item.',
    }
  if (length === 2)
    return {
      name: 'Ultra Rare',
      color: 'success',
      description: 'Highly rare and valuable item.',
    }
  if (length === 3)
    return {
      name: 'Rare',
      color: 'secondary',
      description: 'Rare and valuable item.',
    }
  if (length === 4)
    return {
      name: 'Mythic',
      color: 'warning',
      description: 'Extremely rare and sought-after item.',
    }
  if (length === 5)
    return {
      name: 'Uncommon',
      color: 'success',
      description: 'Less common item with some value.',
    }
  if (length === 6)
    return {
      name: 'Common',
      color: 'danger',
      description: 'Common item with some value.',
    }
  if (length >= 7 && length <= 16)
    return {
      name: 'Basic',
      color: 'warning',
      description: 'Basic item with little value.',
    }
  return {
    name: 'Invalid',
    color: 'default',
    description: 'Rarity cannot be determined.',
  }
}

// FIXME: real
/**
 *
 * @param adatag
 * @param depositBase
 * @param minDeposit
 * @param maxLength the maximumLength the deposit is calculated, afther this length it pays only the minDeposit
 * @returns
 *
 * Default values are:
 * depositBase: 1750 ADA
 * minDeposit: 15 ADA
 * maxLength: 6
 */
export const calculateDeposit = (
  adatag: string,
  depositBase: number,
  minDeposit: number,
  maxLength: number
): bigint => {
  const len = adatag.length

  return len == 0
    ? BigInt(0)
    : len > maxLength
    ? BigInt(minDeposit)
    : BigInt(Math.max(5, (depositBase / 2 ** (adatag.length - 1)) >> 0))
}
