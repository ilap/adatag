import { useState, useEffect, useRef, createContext, useContext } from 'react'
import { isValidUsername, a2h } from '../../utils'
import './Hero.css'
import KoiosTinyClient from 'koios-tiny-client'


const { methods: Koios } = new KoiosTinyClient('https://api.koios.rest/api/v0')

import { Translucent } from "translucent-cardano"

// FIXME: Adahandle's policy ID for testing purposes
const adatagPolicy = 'f0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a'

// Constants for debounce and delay
const debounce = 700 // Debounce in milliseconds
const delay = 1000 // Delay in milliseconds

// Enum for search states
enum SearchState {
  Initial,
  InvalidAdatag,
  AlreadyMinted,
  NotMinted,
  Error,
}

// Captions for different search states
const infoCaption = 'Max 16 characters, starts with a lowercase letter, ends with a letter or a digit, and symbols (-, _, .) are allowed too'
// TODO: const connectCaption = `Connect your wallet to get started`;
const availableCaption = 'Hurray! This adatag is available'
const notAvailableCaption = 'Sorry, this adatag is already taken.'
const errorCaption = 'Sorry, an error occurred while fetching data from the Cardano blockchain.'
const invalidAdatag = 'Sorry, the adatag is invalid. Please follow the guidelines.'

// Function to get caption based on search state
function getCaption (state: SearchState): string {
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

export const Hero = () => {
  const [searchState, setSearchState] = useState<SearchState>(
    SearchState.Initial
  )
  const debounceTimerRef = useRef<number | null>(null)
  const [inputValue, setInputValue] = useState<string>('')
  const [isLoading, setIsLoading] = useState<boolean>(false)

  useEffect(() => {
    // Check if the inputValue is empty
    if (inputValue === '') {
      setSearchState(SearchState.Initial)
      return
    }

    // Clear previous debounce timer
    if (debounceTimerRef.current) {
      clearTimeout(debounceTimerRef.current)
    }

    // Set new debounce timer
    debounceTimerRef.current = window.setTimeout(async () => {
      const valid = isValidUsername(inputValue)
      if (valid) {
        await searchAdatag(inputValue)
      } else {
        setSearchState(SearchState.InvalidAdatag)
      }
    }, debounce)

    // Cleanup function
    return () => {
      // Clear debounce timer on component unmount or re-render
      if (debounceTimerRef.current) {
        clearTimeout(debounceTimerRef.current)
      }
    }
  }, [inputValue])

  // Function to search for adatag
  const searchAdatag = async (value: string) => {
    setIsLoading(true) // Set loading indicator to true
    try {
      await new Promise(resolve => setTimeout(resolve, delay)) // Simulate delay

      const hexString = a2h(value)
      const assetInfo = await Koios.AssetNftAddress({
        _asset_policy: adatagPolicy,
        _asset_name: hexString
    })

      const data = assetInfo?.ok?.data

      if (data) {
        if (data.length > 0) {
          setSearchState(SearchState.AlreadyMinted)
        } else {
          setSearchState(SearchState.NotMinted)
        }
      } else {
        setSearchState(SearchState.Error)
      }
    } catch (error) {
      console.error('Error fetching data:', error)
      setSearchState(SearchState.Error)
    } finally {
      setIsLoading(false) // Set loading indicator back to false after search completes
    }
  }

  // Handle input change
  const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const { value } = event.target
    const adatag =
      value.length === 1
        ? value.replace(/[^a-z]/gi, '')
        : value.replace(/[^a-z\d._-]/gi, '')
    setInputValue(adatag)
  }

  console.log(`Translucent ${Translucent}`)

  const handleButtonClick = () => {
    // Perform the action you want to trigger here
    console.log(`Button clicked! ${inputValue}`);
    //const result = query(`SELECT * from a`, [])
    console.log(`Query: ${JSON.stringify("")}`)
};


  const buttonDisabled = isLoading || searchState != SearchState.NotMinted

  return (
    <div className="container hero h-screen">
      {/* Background Gradients */}
      <div className="purple-gradient" role="presentation" />
      <div className="blue-gradient" role="presentation" />

      <h1>
        Own Your <br />
        digital <span className="highlight">identity</span>
        <br />
        with adat<span className="highlight">@</span>g
      </h1>
      <p className="text-center">
        Create your unique username
        <br />
        with ease
      </p>
      <div className="button-group">
        <div>
          <input
            type="text"
            id="adatag"
            name="adatag"
            onChange={handleChange}
            value={inputValue}
            required
            className="input border-2 border-solid" /*  focus:border-color-brand  focus:outline-none" */
            maxLength={16}
            placeholder="Search @adatag"
          />
        </div>
        <button
          className={`button ${inputValue ? 'cursor-pointer' : 'cursor-not-allowed'}`}
          disabled={buttonDisabled}
          onClick={handleButtonClick} >
          Mint adatag
        </button>
      </div>
      <p className="caption">
        {isLoading
          ? 'Checking on the Cardano Blockchain...'
          : `${getCaption(searchState)}`}
      </p>
    </div>
  )
}
