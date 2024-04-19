import { useState, useEffect, useRef } from 'react'
import { isValidUsername } from '../utils'

export enum SearchState {
  Initial,
  InvalidAdatag,
  AlreadyMinted,
  NotMinted,
  Error,
}

const debounce = 700 // Debounce in milliseconds
const delay = 1000 // Delay in milliseconds

interface UseDebouncedSearchProps {
  checkIfAdatagMinted: (adatag: string) => Promise<boolean>
}

interface UseDebouncedSearchResult {
  inputValue: string
  setInputValue: (adatag: string) => void
  isLoading: boolean
  searchState: SearchState
  handleChange: (event: React.ChangeEvent<HTMLInputElement>) => void
}

const useDebouncedSearch = ({
  checkIfAdatagMinted,
}: UseDebouncedSearchProps): UseDebouncedSearchResult => {
  const [inputValue, setInputValue] = useState<string>('')
  const [isLoading, setIsLoading] = useState<boolean>(false)
  const [searchState, setSearchState] = useState<SearchState>(
    SearchState.Initial,
  )
  const debounceTimerRef = useRef<number | null>(null)

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
      setIsLoading(true)
      const valid = isValidUsername(inputValue)
      if (valid) {
        // Simulate network
        await new Promise(resolve => setTimeout(resolve, delay))
        try {
          const state = (await checkIfAdatagMinted(inputValue))
          ? SearchState.AlreadyMinted
          : SearchState.NotMinted
          setSearchState(state)
        } catch (e) {
          setSearchState(SearchState.Error)
        }  
      } else {
        setSearchState(SearchState.InvalidAdatag)
      }
      setIsLoading(false)
    }, debounce)

    // Cleanup function
    return () => {
      // Clear debounce timer on component unmount or re-render
      if (debounceTimerRef.current) {
        clearTimeout(debounceTimerRef.current)
      }
    }
  }, [inputValue])

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const { value } = event.target
    const adatag =
      value.length === 1
        ? value.replace(/[^a-z]/g, '')
        : value.replace(/[^a-z\d._-]/g, '')
    setInputValue(adatag)
  }

  return {
    inputValue,
    setInputValue,
    isLoading,
    searchState,
    handleChange,
  }
}

export default useDebouncedSearch
