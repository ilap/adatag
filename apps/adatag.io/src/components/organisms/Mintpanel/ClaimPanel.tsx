import React, { useContext, useEffect, useMemo, useState } from 'react'
import { Card, CardHeader, CardBody, CardFooter } from '@nextui-org/react'
//import { AssetExtended } from '@meshsdk/core'

import { Button } from '../../atoms/Button'
import { Input } from '../../atoms/Input'

import { calculateDeposit, getCaption, getRarity } from './utils'
import { checkingCaption, claimguideline } from './constants'
import { WorkerContext } from '../../../context/WorkerContextProvider'
import useDebouncedSearch, { SearchState } from '../../../hooks/useDebouncedSearch'

import { PanelDialog } from './PanelDialog'

import { LoadingSpinner } from './LoadingSpinner'
import { useClaiming } from '../../../hooks/useClaiming'
import ClaimCardHeaderContent from './ClaimCardHeaderContent'

/**
 * 
 * @returns 
Claiming:
1. retrieve the minting tx of the adatag?
Relative cheap 
http://localhost:1442/matches/b3c7c53eb0f654c9b45b77f5c2e002260161056442551749c6b63e10.616469?order=oldest_first&created_after=0

[
  {
     "transaction_id": "1c1d5dc460a0eb28c8de23deb435ce9a1a7719860570fb996f0a7d017e24576a",
...
]


2. Retrieve the transaction retrieved from point 1.
http://localhost:1442/matches/*@1c1d5dc460a0eb28c8de23deb435ce9a1a7719860570fb996f0a7d017e24576a

[
...
  {
    ...
    "address": "addr_test1wpj6qlhku4hfjsxl0vz9n5y4w45xnc7e4qm9h9ccz9hp2sgq34uv4",
    "value": {
      "coins": 437000000,
      "assets": {}
    },
    "datum_hash": "cf518ee8fd97e9f2c494e66fb6a3ccda8d5ecdd77f789cc37474594f6746331c",
    "datum_type": "inline",
  }
...
]

3. retrieve the datum by the datum hash...
http://localhost:1442/datums/cf518ee8fd97e9f2c494e66fb6a3ccda8d5ecdd77f789cc37474594f6746331c

{"datum":"d8799f581c85dd77175f468677da789223823e7249f7c61240b2c50d700df2ea0d1b0000018f0c462638ff"}
Beneficiary ...
121([h'85DD77175F468677DA789223823E7249F7C61240B2C50D700DF2EA0D', 1713897875000])

4. Build a tx. The complete would faild it the beneficiary is different

*/

export const ClaimPanel: React.FC = () => {
  const [isModalOpen, setIsModalOpen] = useState(false)
  const { checkIfAdatagNotMinted } = useContext(WorkerContext)

  const { inputValue, setInputValue, isLoading, searchState, handleChange } = useDebouncedSearch({
    checkAdatag: checkIfAdatagNotMinted,
  })
  const { isClaiming, claimError, claimResult, handleClaim, mintingProgress } = useClaiming()

  const rarity = useMemo(() => getRarity(inputValue.length), [inputValue])
  const buttonDisabled = useMemo(
    () => isClaiming || isLoading || searchState !== SearchState.NotMinted,
    [isClaiming, isLoading, searchState]
  )
  const deposit = useMemo(() => calculateDeposit(inputValue, 1750, 15, 6), [inputValue])

  const isInvalid = searchState === SearchState.Error || searchState === SearchState.InvalidAdatag
  const [donation, setDonation] = useState(0)

  useEffect(() => {
    if (claimResult || claimError) {
      setIsModalOpen(true)
      setInputValue('')
    }
  }, [claimResult, claimError])

  return (
    <>
      {isModalOpen && (
        <PanelDialog
          isOpen={isModalOpen}
          onClose={() => setIsModalOpen(false)}
          mintError={claimError}
          mintResult={claimResult}
        />
      )}
      <Card className="rounded-xl p-4 max-w-[430px] max-h-[580px]">
        <CardHeader className="flex flex-col gap-3">
          <LoadingSpinner isProgressing={isClaiming} content={<>{mintingProgress}</>} />
          <ClaimCardHeaderContent
            inputValue={inputValue}
            rarity={rarity}
            deposit={deposit}
            onDonationHandle={(userDonation: number) => {
              console.log(`IN ONHANDLE: ${userDonation}`)
              setDonation(userDonation)
            }}
          />
        </CardHeader>
        <CardBody>
          <Input
            disabled={isClaiming}
            radius="md"
            variant="bordered"
            isInvalid={isInvalid}
            size="xl"
            color={searchState === SearchState.Minted ? 'orange' : 'default'}
            onChange={handleChange}
            value={inputValue}
            maxLength={16}
            placeholder="Type to search..."
            description={isLoading ? checkingCaption : getCaption(searchState, false)}
            errorMessage={isInvalid ? getCaption(searchState, false) : ''}
          />
          <Button
            size="lg"
            color="primary"
            onPress={() => handleClaim(inputValue, BigInt(donation))}
            className={`${buttonDisabled ? 'cursor-not-allowed' : 'cursor-pointer'}`}
            isDisabled={buttonDisabled}>
            Redeem deposit
          </Button>
        </CardBody>
        <CardFooter>
          <p className="text-small text-center text-default-500 m-0">{claimguideline}</p>
        </CardFooter>
      </Card>
    </>
  )
}
