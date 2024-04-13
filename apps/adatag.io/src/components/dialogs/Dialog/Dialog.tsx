import React, { useState } from 'react'
import { DialogProps, getRarity } from './Dialog.types'
import { CircularProgress } from '@nextui-org/react'
import { AssetExtended } from '@meshsdk/core'
import * as Config from '../../../configs/genesis-config.json'

import {
  Modal,
  ModalContent,
  ModalHeader,
  ModalBody,
  ModalFooter,
  Button,
} from '@nextui-org/react'

import './Dialog.css'
import { InfoIcon } from '../../../assets/svg/info-icon'
import useMinting from '../../../hooks/useMinting'
import { useAssets } from '@meshsdk/react'
import { AdatagMintingService } from '../../../services/MintingService'

const Dialog: React.FC<DialogProps> = ({
  adatag,
  mintButtonClicked,
  setMintButtonClicked,
}) => {
  const assets: AssetExtended[] = useAssets() as unknown as AssetExtended[]
  const [didContinue, setDidContinue] = useState(false)
  const [useAdahandle, setUseAdahandle] = useState(false)

  const { isMinting, mintingProgress, handleMint } = useMinting({ adatag: adatag })

  const closeModal = () => { setMintButtonClicked(false) }

  const handleCheckboxChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setUseAdahandle(event.target.checked)
  }

  const hasAdahandle = assets?.find((asset :AssetExtended) => asset.assetName === adatag && asset.policyId === Config.adatagMinting.params.adahandle)
  const adahandleChecked = useAdahandle && hasAdahandle != undefined

  const rarity = getRarity(adatag.length)
  const deposit = AdatagMintingService.getMinDeposit(adatag)

  return (
    <>
      <Modal
        isOpen={mintButtonClicked && !didContinue}
        onClose={closeModal}
        disableAnimation={true}
        classNames={{
          footer: 'centeredFooter',
        }}
      >
        <ModalContent>
          <ModalHeader>Minting adatag</ModalHeader>
          <ModalBody>
            <div className="minting-dialog__handle">
              <span className="highlight">@</span>
              {adatag}
            </div>
            <div
              className="minting-dialog__rarity"
              style={{ backgroundColor: rarity.color }}
            >
              {rarity.name}
            </div>
            <div>
              Requires deposit of
              <Button isIconOnly size="sm">
                <InfoIcon width={16} height={16} />
              </Button>
            </div>
            <div>{adahandleChecked ? '0₳' : deposit.toString() + '₳'}</div>
            {hasAdahandle && (
              <div className="minting-dialog__adahandle">
                <input
                  type="checkbox"
                  id="use-adahandle"
                  checked={useAdahandle}
                  onChange={handleCheckboxChange}
                />
                <label htmlFor="use-adahandle">
                  Use your $adahandle to avoid time lock deposit.
                </label>
              </div>
            )}
            <div className="minting-dialog__note">
              Note: Additional transaction fees may vary from 0.18A.
            </div>
          </ModalBody>
          <ModalFooter className="footer">
            <Button
              onClick={() => {
                setDidContinue(true)
                setMintButtonClicked(false)
                console.log(`###########: ADAHANDLECHECKED: ${adatag} ... ${adahandleChecked} .... ${deposit}`)
                handleMint(adatag, adahandleChecked, deposit) // Add the required parameters here
              }}
            >
              Mint now
            </Button>
          </ModalFooter>
        </ModalContent>
      </Modal>

      <Modal
        isOpen={didContinue}
        onClose={() => {
          setDidContinue(false)
        }}
        disableAnimation={true}
      >
        <ModalContent>
          <ModalHeader>Minting Progress</ModalHeader>
          <ModalBody>
            {isMinting && (
              <div>
                <CircularProgress color="primary" />
              </div>
            )}
            <div>{mintingProgress}</div>
          </ModalBody>
          <ModalFooter className="footer">
            <Button
              onClick={() => {
                closeModal()
                setDidContinue(false)
              }}
            >
              Close
            </Button>
          </ModalFooter>
        </ModalContent>
      </Modal>
    </>
  )
}

export default Dialog
