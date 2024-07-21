import { useContext, useRef } from 'react'
import { AnimatePresence, motion } from 'framer-motion'
import { Button } from '../../atoms/Button'

import { MintPanel } from '../Mintpanel/Mintpanel'
import { ClaimPanel } from '../Mintpanel/ClaimPanel'

import { heroAnimation, PanelState } from './constants'
import { DialogContext } from '../../../context/ConnectWalletProvider'
import { HowItWorks } from '../HowItWorks/HowItWorks'
import { ScrollButton } from '../../molecules/ScrollButton'
import { useConfig } from '../../../hooks/useConfig'
import React from 'react'
import StatusChip from '../../atoms/StatusChip'

interface Props {
  headerState: 'mint' | 'claim'
  connected: boolean
}

export const Hero: React.FC<Props> = ({ headerState, connected }) => {
  const howtoRef = useRef<HTMLDivElement>(null)

  //const config = useConfig()

  // DEBUG: console.log(`### CONFIG ##### ${JSON.stringify(config)}`)
  const scrollToHow = () => {
    if (howtoRef.current) {
      const navbarHeight = 92
      const targetPosition = howtoRef.current.offsetTop - navbarHeight
      window.scrollTo({ top: targetPosition, behavior: 'smooth' })
    }
  }
  const { toggleDialog } = useContext(DialogContext)

  const statesData: PanelState = {
    default: {
      title: (
        <>
          Own <span className="text-transparent bg-clip-text bg-gradient-to-t from-[#ffcc53] to-[#ffb300]">Your</span>{' '}
          digital
          <br />
          identity with adatag
        </>
      ),
      subTitle: (
        <>
          Create your unique username and connect with others.
          <br />
          Connect wallet to get started
        </>
      ),
      panel: null,
    },
    mint: {
      title: (
        <>
          <span className="text-transparent bg-clip-text bg-gradient-to-t  from-[#ffcc53] to-[#ffb300]">Mint </span>{' '}
          your adatag now
        </>
      ),
      subTitle: <>Never miss a chance to claim your digital identity.</>,
      panel: <MintPanel />,
    },
    claim: {
      title: (
        <>
          <span className="text-transparent bg-clip-text bg-gradient-to-t from-[#ffcc53] to-[#ffb300]">Claim</span> what
          is yours
        </>
      ), // Claim your time-locked deposit
      subTitle: <>When it's time, redeem your time-locked deposit hassle-free.</>,
      panel: <ClaimPanel />,
    },
  }
  const { title, subTitle, panel } = connected ? statesData[headerState] : statesData['default']

  return (
    <>
      <AnimatePresence>
        <motion.section
          className="flex items-center h-screen"
          variants={heroAnimation}
          key={connected ? 'connected' : 'disconnected'}
          initial="initial"
          animate="animate">
          <div className={`container lg:max-w-5xl xl:max-w-6xl flex items-center justify-center mx-auto`}>
            <div className={`${connected ? 'text-left' : 'text-center'}`}>
              {/* text-8xl lg:text-9xl xl:text-10xl font-semibold mb-7 tracking-tighter */}
              <h1 className="text-8xl font-black mb-16 tracking-tighter">{title}</h1>
              <h3 className="text-2xl font-normal mb-12 tracking-tighter">{subTitle}</h3>
              {connected ? null : (
                <Button color="primary" size="xxxl" onClick={toggleDialog}>
                  Connect Wallet
                </Button>
              )}
            </div>
            {panel}
          </div>
          {/*<Status connected={connected} />*/}
          <ScrollButton onClick={scrollToHow} />
          <StatusChip />
        </motion.section>
      </AnimatePresence>
      <div ref={howtoRef}>
        <HowItWorks />
      </div>
    </>
  )
}
