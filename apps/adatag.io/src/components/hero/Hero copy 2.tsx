import { useState, useEffect, useRef, useContext } from 'react'
import { calculateDeposit, isValidUsername } from '../../utils'
import './Hero.css'

import { useWorkerState, WorkerContext, } from '../../context/WorkerContextProvider'
import { getCaption, SearchState } from './Hero.types'
import MintDetailsDialog from '../dialogs/MintDetailsDialog/MintDetailsDialog'
import Dialog from '../dialogs/Dialog/Dialog'
import ProcessLine from '../dialogs/ProcessLine/ProcessLine'


const debounce = 700 // Debounce in milliseconds
const delay = 1000 // Delay in milliseconds

export const Hero = () => {
  const [inputValue, setInputValue] = useState<string>('')
  const [isLoading, setIsLoading] = useState<boolean>(false)
  const chainState = useWorkerState()
  const debounceTimerRef = useRef<number | null>(null)
  const [searchState, setSearchState] = useState<SearchState>(
    SearchState.Initial,
  )
  const { createMintingDetails, checkIfAdatagMinted } = useContext(WorkerContext)

  const [isMintDetailsDialogOpen, setIsMintDetailsDialogOpen] = useState(false);
  const [isOverlayDialogOpen, setIsOverlayDialogOpen] = useState(false);

    useEffect(() => {
    if (!isOverlayDialogOpen) {
      setLineStates({
        0: "idle",
        1: "idle",
        2: "idle",
      });
    }
  }, [isOverlayDialogOpen]);

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

        const state = await checkIfAdatagMinted(inputValue) ? SearchState.AlreadyMinted : SearchState.NotMinted

        setSearchState(state)
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

  // Handle input change
  const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const { value } = event.target
    const adatag =
      value.length === 1
        ? value.replace(/[^a-z]/gi, '')
        : value.replace(/[^a-z\d._-]/gi, '')
    setInputValue(adatag)
  }


  const handleOpenMintDetailsDialog = () => {
    setIsMintDetailsDialogOpen(true);
  };

  const handleCloseMintDetailsDialog = () => {
    setIsMintDetailsDialogOpen(false);
  };

  const handleButtonClick = async () => {
    const { datum, redeemer } = await createMintingDetails('adam1997')
    // Perform the action you want to trigger here
    console.log(`Button clicked! ${inputValue} ${datum}, ${redeemer}`)
    //const result = query(`SELECT * from a`, [])
    console.log(`Query: ${JSON.stringify('')}`)
  }

  const buttonDisabled =  isLoading || searchState !== SearchState.NotMinted

  const handleCloseOverlayDialog = () => {
    setIsOverlayDialogOpen(false);
    setIsMintDetailsDialogOpen(false);
  };

 
  const [lineStates, setLineStates] = useState<
    Record<number, "idle" | "processing" | "ok" | "err">
  >({
    0: "idle",
    1: "idle",
    2: "idle",
  });

  const setLineStatus = (
    id: number,
    status: "idle" | "processing" | "ok" | "err"
  ) => {
    setLineStates((prevStates) => ({ ...prevStates, [id]: status }));
  };

  const allLinesCompleted = () => {
    return Object.values(lineStates).every(
      (status) => status === "ok" || status === "err"
    );
  };

  const processLine1 = async (
    param1: string,
    param2: number
  ): Promise<{ state: "ok" | "err"; params?: any }> => {
    return new Promise((resolve) => {
      setTimeout(() => {
        // Simulate some process and determine the state
        const state = Math.random() < 0.8 ? "ok" : "err";
        const resultParams =
          state === "ok" ? { a: "someValue", b: 42 } : undefined;
        resolve({ state, params: resultParams });
      }, 1000);
    });
  };

  const processLine2 = async (
    paramA: string,
    paramC: number
  ): Promise<{ state: "ok" | "err"; params?: any }> => {
    return new Promise((resolve) => {
      setTimeout(() => {
        // Simulate some process and determine the state
        const state = Math.random() < 0.8 ? "ok" : "err";
        const resultParams =
          state === "ok" ? { d: "anotherValue", e: 84 } : undefined;
        resolve({ state, params: resultParams });
      }, 1000);
    });
  };

  const processFunctions = [processLine1, processLine2, processLine1]; // Add more functions to the array as needed

  const handleMint2 = async () => {
    setIsOverlayDialogOpen(true);
    setIsMintDetailsDialogOpen(false); // Close the MintDetailsDialog

    let params;
    for (let i = 0; i < processFunctions.length; i++) {
      setLineStatus(i, "processing");

      try {
        const result = await processFunctions[i](params?.a, params?.c); // Pass the necessary parameters

        if (result.state === "ok") {
          setLineStatus(i, "ok");
          params = result.params;
        } else {
          setLineStatus(i, "err");
          // Show a system dialog (e.g., an alert)
          alert("An error occurred while processing the line.");
          setIsOverlayDialogOpen(false); // Close the OverlayDialog
          break;
        }
      } catch (error) {
        setLineStatus(i, "err");
        // Show a system dialog (e.g., an alert)
        alert("An error occurred while processing the line.");
        setIsOverlayDialogOpen(false); // Close the OverlayDialog
        break;
      }
    }
  };

  const handleMint = async () => {
    setIsOverlayDialogOpen(true);
    setIsMintDetailsDialogOpen(false); // Close the MintDetailsDialog

      processLine1("1", 1).then(() => {
        setLineStatus(0, 'ok')
        // Handle successful result from a
        processLine2("2", 2).then(resultB => {
          setLineStatus(1, 'ok')
            // Handle successful result from b
            processLine1("3", 3).then(resultC => {
              setLineStatus(2, 'ok')
                // Handle successful result from c
            }).catch(errorC => {
                // Handle error from c
                setLineStatus(2, 'err')
                console.error('An error occurred in c:', errorC);
            });
        }).catch(errorB => {
            // Handle error from b
            setLineStatus(1, 'err')
            console.error('An error occurred in b:', errorB);
        });
    }).catch(errorA => {
        // Handle error from a
        setLineStatus(0, 'err')
        console.error('An error occurred in a:', errorA);
    });
      
    }
  
  
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
        showCloseButton={/*allLinesCompleted()*/true}
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
        
      </div>
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
