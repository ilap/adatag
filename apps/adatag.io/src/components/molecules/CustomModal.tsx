import {
  Modal,
  ModalHeader,
  ModalBody,
  ModalFooter,
  ModalContent,
} from '@nextui-org/react'

import './CustomModal.css'

interface Props {
  isOpen: boolean
  onClose: () => void
  title: string
  children: React.ReactNode
  footer: React.ReactNode
}

export const CustomModal: React.FC<Props> = ({
  isOpen,
  onClose,
  title,
  children,
  footer,
}) => {
  return (
    <Modal
      isOpen={isOpen}
      onClose={onClose}
      scrollBehavior="inside"
      size="md"
      classNames={{
        base: 'rounded-xl max-h-[540px] min-w-[510px]  min-h-[480px] overflow-y-auto',
        closeButton: `m-3 p-3 svg-size-24`,
      }}
    >
      <ModalContent className="p-0">
        {() => (
          <>
            <ModalHeader className=" text-xl p-6">{title}</ModalHeader>
            <ModalBody className="p-6">{children}</ModalBody>
            <ModalFooter className="p-6 justify-center">{footer}</ModalFooter>
          </>
        )}
      </ModalContent>
    </Modal>
  )
}
