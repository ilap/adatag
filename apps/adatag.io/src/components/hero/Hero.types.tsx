// FIXME: Adahandle's policy ID for testing purposes
export const adatagPolicy =
  'f0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a'

// Constants for debounce and delay

// Enum for search states
export enum SearchState {
  Initial,
  InvalidAdatag,
  AlreadyMinted,
  NotMinted,
  Error,
}

// Captions for different search states
export const infoCaption =
  'Max 16 characters, starts with a lowercase letter, ends with a letter or a digit, and symbols (-, _, .) are allowed too'
// TODO: const connectCaption = `Connect your wallet to get started`;
const availableCaption = 'Hurray! This adatag is available'
const notAvailableCaption = 'Sorry, this adatag is already taken.'
const errorCaption =
  'Sorry, an error occurred while fetching data from the Cardano blockchain.'
const invalidAdatag =
  'Sorry, the adatag is invalid. Please follow the guidelines.'

// Function to get caption based on search state
export function getCaption(state: SearchState): string {
  switch (state) {
    case SearchState.Error:
      return errorCaption
    case SearchState.Initial:
      return infoCaption
    case SearchState.InvalidAdatag:
      return invalidAdatag
    case SearchState.AlreadyMinted:
      return notAvailableCaption
    case SearchState.NotMinted:
      return availableCaption
    default:
      return infoCaption
  }
}
