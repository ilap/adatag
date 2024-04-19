import {  Modal, ModalHeader, ModalBody, ModalFooter, ModalContent, } from '@nextui-org/react'
import { Button } from '../atoms/Button'

import './CustomModal.css'

interface CustomModalProps {
  isOpen: boolean
  onClose: () => void
  title: string
  children: any
  footer: any
}

const CustomModal: React.FC<CustomModalProps> = ({
  isOpen,
  onClose,
  title,
  children,
  footer
}) => {


  return (
    <Modal 
      isOpen={isOpen} 
      onClose={onClose}
      scrollBehavior="inside"
      size="md"
      classNames={{
        base: "max-h-[540px] min-w-[510px]  min-h-[480px] overflow-y-auto rounded-3xl",
        closeButton: `m-3 p-3 svg-size-24`
      }}
     >
      <ModalContent className="p-0">
        {onClose => (
          <>
            <ModalHeader className="p-6">{title}</ModalHeader>
            <ModalBody className="p-6">
              {children}
            </ModalBody>
            <ModalFooter className="p-6 justify-center">
              {footer}
            </ModalFooter>
          </>
        )}
      </ModalContent>
    </Modal>
  )
}

export default CustomModal
