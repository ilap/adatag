import { Card, CardBody, CardFooter, Switch, Tooltip, Chip } from '@nextui-org/react'
import { InformationCircleIcon } from '@heroicons/react/24/outline'
import { ResponsiveText } from '../../atoms/ResponsiveText'
import { timelockTooltip } from './constants'
import { Rarity } from './types'
import React from 'react'

interface Props {
  inputValue: string
  rarity: Rarity
  useAdahandle: boolean
  hasAdahandle: boolean
  adahandleChecked: boolean
  deposit: bigint
  formattedDate: string
  onToggleAdahandle: () => void
}

export const CardHeaderContent: React.FC<Props> = ({
  inputValue,
  rarity,
  useAdahandle,
  hasAdahandle,
  adahandleChecked,
  deposit,
  formattedDate,
  onToggleAdahandle,
}) => (
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
            <p className="text-xl m-0">Time lock deposit</p>
            <Tooltip size="lg" color="foreground" content={<div className="text-medium text-wrap overflow-hidden max-w-sm whitespace-pre-wrap break-all">{timelockTooltip}</div>}>
              <InformationCircleIcon className="h-5 w-5 cursor-pointer" />
            </Tooltip>
          </div>
          <div>
            <span className="font-bold text-xl m-0">₳&nbsp;{adahandleChecked ? '0' : deposit.toString()}</span>
          </div>
        </div>
        <div className="flex items-center pt-2">
          <div className="min-h-12 max-h-12 overflow-hidden flex-grow flex items-center">
            <p className="whitespace-normal text-xl m-0">
              {!hasAdahandle ? 'Claim deposit after' : 'Use your ada handle to avoid deposit'}
            </p>
          </div>
          {!hasAdahandle ? (
            <p className="font-bold text-xl m-0">{formattedDate}</p>
          ) : (
            <Switch color="warning" size="lg" isSelected={useAdahandle} onValueChange={onToggleAdahandle} />
          )}
        </div>
      </div>
    </CardFooter>
  </Card>
)
