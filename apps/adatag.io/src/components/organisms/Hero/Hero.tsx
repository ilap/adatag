import React, { useContext, useEffect, useState } from 'react'
import { Button } from '../../atoms/Button'
import { Mintpanel } from '../Mintpanel/Mintpanel'
import { AnimatePresence, motion } from 'framer-motion'
import { heroAnimation } from './constants'
import { DialogContext } from '../../../context/ConnectWalletProvider'


const HeroSection = ({ connected }: { connected: boolean }) => {
  const titleLines = connected
    ? 'Mint your\nadatag now'
    : 'Own Your digital\nidentity with adatag'
  const subTitleLines = connected
    ? 'Create your unique username and connect with others.'
    : 'Create your unique username with ease.\nConnect wallet to get started'
  const justify = connected ? 'between' : 'center'

  const renderTitle = titleLines.split('\n').map((line, index) => (
    <React.Fragment key={index}>
      {line}
      <br />
    </React.Fragment>
  ))
  const renderSubTitle = subTitleLines.split('\n').map((line, index) => (
    <React.Fragment key={index}>
      {line}
      <br />
    </React.Fragment>
  ))

  //const [isModalOpen, setIsModalOpen] = useState(false)


  /*useEffect(() => {
    if (connected) {
      setIsModalOpen(false);
    }
  }, [connected]);

  const toggleModal = () => {
    setIsModalOpen(!isModalOpen);
  };
*/
  const { toggleDialog } = useContext(DialogContext);
  /*
      {isModalOpen && (
        <ConnectWalletDialog
          isOpen={isModalOpen}
          onClose={toggleModal}
        />
      )}

      ....
                      <Button size="xxxl" onClick={toggleModal}>
                  Connect Wallet
                </Button>
  */
  return (
    <>
  

      <AnimatePresence>
        <motion.section
          className="flex items-center h-screen"
          variants={heroAnimation}
          key={connected ? 'connected' : 'disconnected'}
          initial="initial"
          animate="animate"
        >
          <div
            className={`flex items-center justify-${justify} min-w-[1152px] mx-auto`}
          >
            <div className={`${connected ? 'text-left' : 'text-center'}`}>
              <h1 className="text-8xl font-black mb-16 tracking-tighter">
                {renderTitle}
              </h1>
              <h3 className="text-2xl font-normal mb-12 tracking-tighter">
                {renderSubTitle}
              </h3>
              {!connected && (
                <>

                                <Button size="xxxl" onClick={toggleDialog}>
                                Connect Wallet
                              </Button>
                              </>
              )}
            </div>
            {connected && <Mintpanel />}
          </div>
          {/*<Status connected={connected} />*/}
        </motion.section>
      </AnimatePresence>
    </>
  )
}


export default HeroSection
