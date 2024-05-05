import {
  Card,
  CardBody,
  CardFooter,
  Chip,
  //Button,
  Input,
} from '@nextui-org/react'
import { ResponsiveText } from '../../atoms/ResponsiveText'
import { Rarity } from './types'
import { useState } from 'react'
import { Button } from '../../atoms/Button'

interface Props {
  inputValue: string
  rarity: Rarity
  deposit: bigint
  onDonationHandle: (donation: number) => void
}

const ClaimCardHeaderContent: React.FC<Props> = ({ inputValue, rarity, deposit, onDonationHandle }) => {
  const [selectedButton, setSelectedButton] = useState<number | null>(null)
  const [value, setValue] = useState('')

  const handleButtonClick = (value: number) => {
    const donation = selectedButton === value ? 0 : value
    setSelectedButton(donation)
    console.log(`Button value changes VALUE: ${value}`)
    onDonationHandle(donation || 0)
    setValue('')
  }

  const handleDonationChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const { value } = event.target
    console.log(`WWWWW VALUE: ${value}`)
    const donation = value.replace(/[^0-9]/g, '')
    console.log(`WWWWW VALUE: ${value} ... ${donation}`)
    setValue(donation)
    onDonationHandle(Number(donation))
  }

  const redeemableValue = selectedButton
    ? Math.max(0, Number(deposit) - selectedButton)
    : Math.max(0, Number(deposit) - (Number(value) || 0))

  return (
    <Card fullWidth shadow="none" radius="none" className="flex justify-center items-center relative">
      <CardBody className="flex items-center justify-center min-w-[300px] min-h-[190px] p-0 rounded-xl ">
        <div className="p-4">
          <div className="p-4 min-w-[200px] whitespace-normal text-center">
            <ResponsiveText text={inputValue} />
          </div>
        </div>
        <Chip size="lg" radius="full" variant="flat" color={rarity.color} className="p-4">
          {rarity.name}
        </Chip>
      </CardBody>
      <CardFooter className="p-0">
        <div className="flex flex-col md:flex-col w-full">
          <div className="flex items-center justify-between pt-2">
            <div className="flex items-center">
              <p className="text-xl m-0">Claimable value: </p>
            </div>
            <div>
              <span className="font-bold text-xl m-0">₳&nbsp;{redeemableValue.toString()}</span>
            </div>
          </div>
          <div className="flex items-center pt-2">
            <div className="min-h-12 max-h-12 overflow-hidden flex-grow flex justify-center items-center gap-1">
              <p className="text-xl m-0 pr-3">Donate: </p>
              <Button
                //variant={selectedButton === 2 ? 'solid' : 'ghost'}

                radius="full"
                color={selectedButton === 2 ? 'primary' : 'connect'}
                className={`min-w-12 max-w-12 font-medium ${selectedButton !== 2 && 'border-2 border-opacity-50% hover:border-foreground'}`}
                size="lg"
                onClick={() => handleButtonClick(2)}>
                ₳ 2
              </Button>
              <Button
                radius="full"
                color={selectedButton === 5 ? 'primary' : 'connect'}
                className={`min-w-12 max-w-12 font-medium ${selectedButton !== 5 && 'border-2 border-opacity-50% hover:border-foreground'}`}
                size="lg"
                onClick={() => handleButtonClick(5)}>
                ₳ 5
              </Button>
              <Button
                radius="full"
                color={selectedButton === 15 ? 'primary' : 'connect'}
                className={`min-w-12 max-w-12 font-medium ${selectedButton !== 15 && 'border-2 border-opacity-50% hover:border-foreground'}`}
                size="lg"
                onClick={() => handleButtonClick(15)}>
                ₳ 15
              </Button>

              <Input
                maxLength={deposit.toString().length}
                className="pl-3 text-large"
                radius="lg"
                variant="bordered"
                size="lg"
                placeholder="Custom"
                startContent={
                  <div className="pointer-events-none flex items-center">
                    <span className="text-default-800  text-medium">₳</span>
                  </div>
                }
                value={value}
                onChange={e => {
                  handleDonationChange(e)
                  setSelectedButton(null)
                }}
              />
            </div>
          </div>
        </div>
      </CardFooter>
    </Card>
  )
}

export default ClaimCardHeaderContent
