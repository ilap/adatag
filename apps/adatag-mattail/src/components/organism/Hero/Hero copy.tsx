import { useContext, useEffect, useReducer, useMemo, useCallback } from 'react';
import { InformationCircleIcon } from '@heroicons/react/24/outline';
import { Card, CardHeader, CardBody, CardFooter, Switch, Tooltip, Chip, CircularProgress } from '@nextui-org/react';
import { Button } from '../../atoms/Button';
import { Input } from '../../atoms/Input';
import ResponsiveText from '../../atoms/ResponsiveText';
import { useAssets } from '@meshsdk/react';
import useMinting from '../../../hooks/useMinting';
import CustomModal from '../../molecules/InformationDialog';
import { calculateDeposit, getRarity } from './utils';
import { checkingCaption, guideline, timelockTooltip } from './constants';
import { WorkerContext } from '../../../context/WorkerContextProvider';
import useDebouncedSearch, { SearchState } from '../../../hooks/useDebouncedSearch';
import * as Config from '../../../configs/genesis-config.json';
import { Rarity } from './types';

const initialState = {
  inputValue: '',
  isModalOpen: false,
  useAdahandle: false,
};

const reducer = (state, action) => {
  switch (action.type) {
    case 'TOGGLE_MODAL':
      return { ...state, isModalOpen: !state.isModalOpen };
    case 'TOGGLE_ADAHANDLE':
      return { ...state, useAdahandle: !state.useAdahandle };
    default:
      return state;
  }
};

export const Mintpanel = () => {
  const [state, dispatch] = useReducer(reducer, initialState);
  const { inputValue, isModalOpen, useAdahandle } = state;

  const assets = useAssets();
  const { checkIfAdatagMinted } = useContext(WorkerContext);
  const { isLoading, searchState, handleChange } = useDebouncedSearch({ checkIfAdatagMinted });
  const { isMinting, mintError, mintResult, handleMint } = useMinting({ adatag: inputValue });

  const rarity = useMemo(() => getRarity(inputValue.length), [inputValue]);
  const buttonDisabled = useMemo(() => isMinting || isLoading || searchState !== SearchState.NotMinted, [isMinting, isLoading, searchState]);
  const deposit = useMemo(() => calculateDeposit(inputValue, 1750, 15, 6), [inputValue]);
  const date = useMemo(() => new Date(Date.now() + 20 * 86400 * 1000), []);
  const formattedDate = useMemo(() => date.toLocaleDateString('en-US', { year: 'numeric', month: '2-digit', day: '2-digit' }), [date]);
  const hasAdahandle = useMemo(() => assets.some(asset => asset.assetName === inputValue && asset.policyId === Config.adatagMinting.params.adahandle), [assets, inputValue]);
  const adahandleChecked = useMemo(() => useAdahandle && hasAdahandle, [useAdahandle, hasAdahandle]);
  const isInvalid = useMemo(() => searchState === SearchState.Error || searchState === SearchState.InvalidAdatag, [searchState]);

  const toggleModal = useCallback(() => dispatch({ type: 'TOGGLE_MODAL' }), []);
  const toggleAdahandle = useCallback(() => dispatch({ type: 'TOGGLE_ADAHANDLE' }), []);

  useEffect(() => {
    if (mintResult || mintError) {
      toggleModal();

    }
  }, [mintResult, mintError, toggleModal]);

  return (
    <>
      <CustomModal
        isOpen={isModalOpen}
        onClose={toggleModal}
        title={mintResult ? 'Success!' : 'Error'}
        subtitle={mintResult ? `Your operation was successful.${mintResult}` : `There was an error with your operation.${mintError?.message}`}
        state={mintResult ? 'success' : 'error'}
      />
      <Card radius="lg" className="p-4 max-w-[430px] max-h-[580px]">
        <CardHeader className="flex flex-col gap-3">
          <LoadingSpinner isLoading={isLoading || isMinting} />
          <CardHeaderContent
            inputValue={inputValue}
            rarity={rarity}
            useAdahandle={useAdahandle}
            hasAdahandle={hasAdahandle}
            adahandleChecked={adahandleChecked}
            deposit={deposit}
            formattedDate={formattedDate}
            onToggleAdahandle={toggleAdahandle}
          />
        </CardHeader>
        <CardBody>
          <Input
            disabled={isMinting}
            radius="lg"
            variant="bordered"
            isInvalid={isInvalid}
            size="xl"
            color={searchState === SearchState.AlreadyMinted ? 'orange' : 'default'}
            onChange={handleChange}
            value={inputValue}
            maxLength={16}
            placeholder="Start typing here..."
            description={isLoading ? checkingCaption : `${getCaption(searchState)}`}
            className="h-unit-22"
            errorMessage={isInvalid ? getCaption(searchState) : ''}
          />
          <Button
            size="lg"
            onPress={() => { handleMint(inputValue, adahandleChecked, deposit); }}
            className={`${buttonDisabled ? 'cursor-not-allowed' : 'cursor-pointer'}`}
            isDisabled={buttonDisabled}
          >
            Mint adatag
          </Button>
        </CardBody>
        <CardFooter>
          <p className="text-small text-center text-default-500">{guideline}</p>
        </CardFooter>
      </Card>
    </>
  );
};

const LoadingSpinner = ({ isLoading }) => (
  isLoading && (
    <div className="absolute inset-0 p-4 flex flex-col items-center z-10 justify-center bg-white bg-opacity-75 backdrop-blur-sm">
      <div className="flex items-center justify-center w-full">
        <Button isIconOnly className="cursor-pointer absolute top-1 right-1" onClick={() => { }} />
      </div>
      <CircularProgress size="lg" className="p-5" />
      <div className="font-bold text-xl p-5">Working on it</div>
    </div>
  )
);

const CardHeaderContent = ({
  inputValue,
  rarity,
  useAdahandle,
  hasAdahandle,
  adahandleChecked,
  deposit,
  formattedDate,
  onToggleAdahandle
}) => (
  <Card
    fullWidth
    shadow="none"
    radius="none"
    className="flex justify-center items-center relative"
  >
    <CardBody className="flex items-center justify-center min-w-[300px] min-h-[190px] p-0 rounded-xl ">
      <div className="p-4">
        <div className="p-4 min-w-[200px] whitespace-normal text-center">
          <ResponsiveText text={inputValue} />
        </div>
      </div>
      <Chip
        size="lg"
        radius="full"
        variant="flat"
        color={rarity.color}
        className="p-4"
      >
        {rarity.name}
      </Chip>
    </CardBody>
    <CardFooter className="p-0">
      <div className="flex flex-col md:flex-col w-full">
        <div className="flex items-center justify-between pt-2">
          <div className="flex items-center">
            <p className="text-xl">Time lock deposit</p>
            <Tooltip
              showArrow
              content={
                <div className="max-w-44 text-small">
                  {timelockTooltip}
                </div>
              }
            >
              <InformationCircleIcon className="h-5 w-5 cursor-pointer" />
            </Tooltip>
          </div>
          <div>
            <span className="font-bold text-xl">
              ₳&nbsp;{adahandleChecked ? '0' : deposit.toString()}
            </span>
          </div>
        </div>
        <div className="flex items-center pt-2">
          <div className="min-h-12 max-h-12 overflow-hidden flex-grow flex items-center">
            <p className="whitespace-normal text-xl">
              {!hasAdahandle ? 'Claim deposit after' : 'Use your ada handle to avoid deposit'}
            </p>
          </div>
          {!hasAdahandle ? (
            <p className="font-bold text-xl">{formattedDate}</p>
          ) : (
            <Switch
              color="warning"
              size="lg"
              isSelected={useAdahandle}
              onValueChange={onToggleAdahandle}
            />
          )}
        </div>
      </div>
    </CardFooter>
  </Card>
);
