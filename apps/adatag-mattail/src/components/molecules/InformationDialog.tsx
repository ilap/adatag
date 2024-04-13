import {
  Modal,
  ModalHeader,
  ModalBody,
  ModalFooter,
  Button,
  ModalContent,
} from '@nextui-org/react'
import {
  FaCheckCircle,
  FaExclamationCircle,
  FaExclamationTriangle,
} from 'react-icons/fa'

interface CustomModalProps {
  isOpen: boolean
  onClose: () => void
  title: string
  subtitle: string
  state: 'success' | 'error' | 'warning'
}

const CustomModal: React.FC<CustomModalProps> = ({
  isOpen,
  onClose,
  title,
  subtitle,
  state,
}) => {
  let icon
  let color:
    | 'success'
    | 'danger'
    | 'warning'
    | 'default'
    | 'primary'
    | 'secondary'
    | undefined
  switch (state) {
    case 'success':
      icon = <FaCheckCircle size={32} />
      color = 'success'
      break
    case 'error':
      icon = <FaExclamationCircle size={32} />
      color = 'danger'
      break
    case 'warning':
      icon = <FaExclamationTriangle size={32} />
      color = 'warning'
      break
    default:
      icon = <FaExclamationCircle size={32} />
      color = 'danger'
  }


  return (
    <Modal isOpen={isOpen} onClose={onClose}>
      <ModalContent>
        {onClose => (
          <>
            <ModalHeader>{title}</ModalHeader>
            <ModalBody>
              <div style={{ display: 'flex', alignItems: 'center' }}>
                {icon}
                <div style={{ marginLeft: '10px' }}>{subtitle}</div>
              </div>
            </ModalBody>
            <ModalFooter>
              <Button onClick={onClose} color={color}>
                Close
              </Button>
            </ModalFooter>
          </>
        )}
      </ModalContent>
    </Modal>
  )
}

export default CustomModal
