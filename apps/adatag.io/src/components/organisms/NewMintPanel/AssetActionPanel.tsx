/* TODO: implement generic mint panel. 
import { useReducer, useCallback } from 'react'
import { Card, CardHeader, CardBody, CardFooter } from '@nextui-org/react'
import { Button } from '../../atoms/Button'
import { CustomModal } from '../../molecules/CustomModal'
import { Action, initialState, State } from './types'
import { ResponsiveText } from '../../atoms/ResponsiveText'
import { AssetExtended } from '@meshsdk/core'

interface AssetActionPanelProps {
  title: string
  buttonText: string
  onAction: (asset: AssetExtended | undefined) => void
  children: React.ReactNode
}

const reducer = (state: State, action: Action): State => {
  switch (action.type) {
    case 'OPEN_MODAL':
      return { ...state, isModalOpen: true }
    case 'CLOSE_MODAL':
      return { ...state, isModalOpen: false }
    case 'TOGGLE_ADAHANDLE':
      return { ...state, useAdahandle: !state.useAdahandle }
    default:
      return state
  }
}

export const AssetActionPanel = ({
  title,
  buttonText,
  onAction,
  children,
}: AssetActionPanelProps) => {
  const [state, dispatch] = useReducer(reducer, initialState)
  const { isModalOpen } = state

  const openModal = useCallback(() => {
    dispatch({ type: 'OPEN_MODAL' })
  }, [])

  const closeModal = useCallback(() => {
    dispatch({ type: 'CLOSE_MODAL' })
  }, [])

  const handleAction = useCallback(() => {
    onAction(state.selectedAsset)
    closeModal()
  }, [onAction, state.selectedAsset, closeModal])

  return (
    <>
      <Card radius="lg" className="p-4 max-w-[430px] max-h-[580px]">
        <CardHeader>
          <ResponsiveText text={title} />
        </CardHeader>
        <CardBody>{children}</CardBody>
        <CardFooter>
          <Button size="lg" onPress={openModal}>
            {buttonText}
          </Button>
        </CardFooter>
      </Card>
      <CustomModal
        isOpen={isModalOpen}
        onClose={closeModal}
        title={title}
        footer={
          <Button size="lg" onPress={handleAction}>
            {buttonText}
          </Button>
        }>
        {children}
      </CustomModal>
    </>
  )
}
*/
