import React, { useState } from 'react';
import ReactModal from 'react-modal';
import './MintDetailsDialog.css';
import { CloseIcon } from '../../../assets/svg/close-icon';
import { InfoIcon } from '../../../assets/svg/info-icon';
import { getRarity, MintDetailsDialogProps } from './MintDetailsDialog.types';



const MintDetailsDialog: React.FC<MintDetailsDialogProps> = ({
  isOpen,
  onClose,
  onMint,
  adatag,
  deposit,
  handleOpenProgressDialog
}) => {
  const [useAdahandle, setUseAdahandle] = useState(false);

  const handleMint = () => {
    handleOpenProgressDialog(/*ad*/);
    onMint();
  };

  const handleClose = () => {
    onClose();
  };

  const handleCheckboxChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setUseAdahandle(event.target.checked);
  };

  const adahandleChecked = useAdahandle && adatag === 'yourhandle';
  const rarity = getRarity(adatag.length)


  return (
    <ReactModal
      isOpen={isOpen}
      onRequestClose={handleClose}
      className="minting-dialog minting-dialog--larger-margin"
      overlayClassName="overlay"
    >
      <div className="mint-dialog-header">
        <button className="minting-dialog__icon-button" onClick={onClose}>
          <CloseIcon width={32} height={32} />
        </button>
      </div>
      <div className="mint-dialog-content">
        <div className="minting-dialog__top-content">
          <h2 className="minting-dialog__title">Minting</h2>
          <div className="minting-dialog__handle-rarity">
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
          </div>
        </div>
        <div className="minting-dialog__middle-content">
          <div className="minting-dialog__deposit">
            <div>
              <div className="minting-dialog__deposit-label">
                Requires deposit of
                <button className="minting-dialog__icon-button">
                  <InfoIcon width={16} height={16} />
                </button>
              </div>
            </div>
          </div>
          <div className="minting-dialog__deposit-amount">
            {adahandleChecked ? '0₳' : deposit+'₳'}
          </div>
        </div>
        <div className="minting-dialog__bottom-content">
        {adatag === 'yourhandle' && (
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
        </div>
      </div>
      <div className="mint-dialog-footer">
        <button className="minting-dialog__button minting-dialog__button--mint" onClick={handleMint}>
          Mint your adatag
        </button>
      </div>
    </ReactModal>
  );
};

export default MintDetailsDialog;
