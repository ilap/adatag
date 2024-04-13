import { useContext, useRef, useState } from 'react'
import { Button } from '@nextui-org/react'

import { useWorkerState, WorkerContext } from '../../context/WorkerContextProvider'

import './Hero.css'
import { getCaption, SearchState } from './Hero.types'
import useDebouncedSearch from '../../hooks/useDebouncedSearch'

import Dialog from '../dialogs/Dialog/Dialog'
import useMinting from '../../hooks/useMinting'
import { useWallet } from '@meshsdk/react'

export const Hero = () => {
  const inputRef = useRef<HTMLInputElement>(null);
  const { connected } = useWallet()

  const [ mintButtonClicked, setMintButtonClicked] = useState(false)
  const [ adatag, setAdatag] = useState('')
  const chainState = useWorkerState()
  const { checkIfAdatagMinted } = useContext(WorkerContext)

  const {
    inputValue,
    setInputValue,
    isLoading,
    searchState,
    handleChange,
  } = useDebouncedSearch({ checkIfAdatagMinted })

  const buttonDisabled = isLoading || searchState !== SearchState.NotMinted

  const openDialog = (() => {
    setMintButtonClicked(true)
  }) 

  const clearInput = () => {
    setAdatag(inputValue)
    setInputValue('');
  };

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
        {connected && (
        <><div>
            <input
              ref={inputRef}
              type="text"
              id="adatag"
              name="adatag"
              onChange={handleChange}
              value={inputValue}
              required
              className="input border-2 border-solid" /*  focus:border-color-brand  focus:outline-none" */
              maxLength={16}
              placeholder="Search @adatag" />
          </div><Button
            className={`button ${buttonDisabled ? 'cursor-not-allowed' : 'cursor-pointer'}`}
            disabled={buttonDisabled}
            onClick={() => {
              openDialog();
              clearInput(); // Call the clearInput function to clear the input value
            }}
          >
              Mint adatag
            </Button></>
        )}
        { !connected && (
          <>
          <p>Connect wallet to get started</p>
          </>
        )}
        </div>
        <p className="caption">
        {isLoading
          ? 'Checking on the Cardano Blockchain...'
          : `${getCaption(searchState)}`}
        </p>
        <Dialog
        mintButtonClicked={mintButtonClicked}
        setMintButtonClicked={setMintButtonClicked} 
        adatag={adatag}
        />
      <p className="caption">
        {`State ${chainState.state}, message: ${chainState.message} ..... `}
      </p>
    </div>
  )
}
