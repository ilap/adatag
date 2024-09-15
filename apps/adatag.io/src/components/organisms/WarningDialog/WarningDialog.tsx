import React, { useRef } from 'react'
import { Button } from '../../atoms/Button'
import { CustomModal } from '../../molecules/CustomModal'
import { DialogContent } from '../../molecules/DialogContent'
import useLocalStorage from '../../../hooks/useLocalStorage'
import { Link } from '@nextui-org/react'

interface Props {
  title: string
  subtitle: string
  isOpen: boolean
  onClose: () => void
}

export const WarningDialog: React.FC<Props> = ({ title, subtitle, isOpen, onClose }) => {
  const [showDialog, setShowDialog] = useLocalStorage('showWarningDialog', true)
  const checkboxRef = useRef<HTMLInputElement>(null)

  const handleClose = () => {
    onClose()
  }

  const handleCheckboxChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    if (event.target.checked) {
      setShowDialog(false)
    }
  }

  const renderContent = () => {
    return (
      <DialogContent
        icon={undefined}
        title=""
        subtitle={
          <div>
            <p>
              This is a PoC project for <strong>Adatag</strong>, using Plutus scripts on the{' '}
              <strong>Cardano Preview Testnet</strong>. It relies on <strong>demeter.run</strong> public endpoints under
              a free subscription, which allows a maximum of
              <strong> 1 concurrent open connection</strong>. As a result, the service may experience errors, slowdowns,
              or potential misuse. If issues occur, please try again later or report them on our{' '}
              <strong>
                <i>
                  <Link href="https://github.com/ilap/adatag/issues">GitHub page</Link>
                </i>
              </strong>
              .
            </p>
            <br />
            <p>
              Only <strong>Brave</strong> and <strong>Chrome</strong> browsers are supported, along with{' '}
              <strong>Conway-compatible wallet extensions</strong> like the latest versions of <strong>Lace</strong> and{' '}
              <strong>Yoroi</strong>.
              <br />
              <br />
              Make sure your browser and wallet extension are up to date, and that your wallet is connected to the
              Preview network to avoid compatibility issues.
            </p>
          </div>
        }
      />
    )
  }

  const renderFooter = () => {
    return (
      <div style={{ display: 'flex', flexDirection: 'column', alignItems: 'center', width: '100%' }}>
        <label style={{ marginBottom: '10px', width: '100%', textAlign: 'center' }}>
          <input type="checkbox" ref={checkboxRef} onChange={handleCheckboxChange} style={{ marginRight: '5px' }} />
          Don't show this again
        </label>
        <Button fullWidth color="primary" onClick={handleClose} size="lg">
          Ok
        </Button>
      </div>
    )
  }

  return isOpen ? (
    <CustomModal isOpen={isOpen} onClose={handleClose} title="Warning!" footer={renderFooter()}>
      {renderContent()}
    </CustomModal>
  ) : null
}
