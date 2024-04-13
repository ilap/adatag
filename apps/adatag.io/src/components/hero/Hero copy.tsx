import React, { useContext } from 'react'
import { useWorkerState, WorkerContext } from '../../context/WorkerContextProvider'
import { getCaption, SearchState } from './Hero.types'
import MintDetailsDialog from '../dialogs/MintDetailsDialog/MintDetailsDialog'
import Dialog from '../dialogs/Dialog/Dialog'
import ProcessLine from '../dialogs/ProcessLine/ProcessLine'
import useDebouncedSearch from '../../hooks/useDebouncedSearch'
import useProcessLines from '../../hooks/useProcessLines'
import useMintDetailsDialog from '../../hooks/useMintDetailsDialog'
import { calculateDeposit } from '../../utils'

export const Hero = () => {
  const chainState = useWorkerState()
  const { createMintingDetails, checkIfAdatagMinted } = useContext(WorkerContext)

  const {
    inputValue,
    isLoading,
    searchState,
    handleChange,
  } = useDebouncedSearch({ checkIfAdatagMinted })

  const {
    isMintDetailsDialogOpen,
    handleOpenMintDetailsDialog,
    handleCloseMintDetailsDialog,
    handleMint,
  } = useMintDetailsDialog({createMintingDetails, inputValue})

  const {
    isOverlayDialogOpen,
    handleCloseOverlayDialog,
    lineStates,
    setLineStatus,
    allLinesCompleted,
  } = useProcessLines({handleMint, isMintDetailsDialogOpen, handleCloseMintDetailsDialog})

  const buttonDisabled = isLoading || searchState !== SearchState.NotMinted

  
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
          className={`button ${buttonDisabled ? 'cursor-not-allowed' : 'cursor-pointer' }`}
          disabled={buttonDisabled}
          onClick={handleOpenMintDetailsDialog/*handleButtonClick*/}
        >
          Mint adatag
        </button>
        </div>
        <MintDetailsDialog
          isOpen={isMintDetailsDialogOpen}
          onClose={handleCloseMintDetailsDialog}
          onMint={/*handleButtonClick*/handleMint}
          adatag={inputValue}
          deposit={calculateDeposit(inputValue, 1750, 5,5).toString()}
        />
        <Dialog
        isOpen={isOverlayDialogOpen}
        onClose={handleCloseOverlayDialog}
        title="Overlay Dialog"
        showCloseButton={allLinesCompleted()}
        >
        <ProcessLine
          id={0}
          status={lineStates[0]}
          description="Line 1 description"
          setStatus={setLineStatus.bind(null, 0)}
        />
        <ProcessLine
          id={1}
          status={lineStates[1]}
          description="Line 2 description"
          setStatus={setLineStatus.bind(null, 1)}
        />
        <ProcessLine
          id={2}
          status={lineStates[2]}
          description="Line 3 description"
          setStatus={setLineStatus.bind(null, 2)}
        />
      </Dialog>

      <p className="caption">
        {isLoading
          ? 'Checking on the Cardano Blockchain...'
          : `${getCaption(searchState)}`}
      </p>
      <p className="caption">
        {`State ${chainState.state}, message: ${chainState.message}`}
      </p>
    </div>
  )
}
