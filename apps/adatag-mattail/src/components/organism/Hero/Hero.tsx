import React from 'react';
import { Button } from '../../atoms/Button';
import { Mintpanel } from '../Mintpanel/Mintpanel';
import { AnimatePresence, motion } from 'framer-motion';


const variants = {
  initial: {
    opacity: 0,
  },
  animate: {
    opacity: 1,
    transition: { duration: 1.5 },
  },
  exit: {
    opacity: 0,
    transition: { duration: 1.5 },
  },
};

const HeroSection = ({ connected }: { connected: boolean }) => {
  const titleLines = connected ? 'Mint your\nadatag now' : 'Own Your digital\nidentity with adatag';
  const subTitleLines = connected ? "Create your unique username and connect with others." : "Create your unique username with ease.\nConnect wallet to get started";
  const justify = connected ? 'between' : 'center'
  const renderTitle = titleLines.split('\n').map((line, index) => <React.Fragment key={index}>{line}<br /></React.Fragment>);
  const renderSubTitle = subTitleLines.split('\n').map((line, index) => <React.Fragment key={index}>{line}<br /></React.Fragment>);

  return (
    <AnimatePresence>
    <motion.section className="flex items-center h-screen" variants={variants} 
    key={ connected ? "connected" : "disconnected"}
    initial="initial" animate="animate">
      <div className={`flex items-center justify-${justify} min-w-[1152px] mx-auto`}>
        <div className={`${connected ? "text-left" : "text-center"}`}>
          <h1 className="text-8xl font-black mb-16 tracking-tighter">{renderTitle}</h1>
          <h3 className="text-2xl font-normal mb-12 tracking-tighter">{renderSubTitle}</h3>
          {!connected && <Button size="xxxl">Connect Wallet</Button>}
        </div>
        {connected && <Mintpanel />}
      </div>
      {/*<Status connected={connected} />*/}
    </motion.section>
    </AnimatePresence>
  );
}

const Status = ({ connected }: { connected: boolean }) => {
  const backgroundColor = connected ? '#00FF00' : '#FF0000'
  const textColor = connected ? '#000000' : '#FFFFFF'
  const text = connected ? 'Connected' : 'Disconnected'

  return (
    <><div className='absolute bottom-10 left-10'>
    <div
      className="rounded-full px-4 py-2 text-sm font-medium"
      style={{ backgroundColor, color: textColor }}
    >
      {text}
    </div>
    <div>
    <span className='relative flex h-2 w-2'>
      <span className= ' bg-orange-900 absolute -top-1 -left-1 inline-flex h-4 w-4 animate-ping rounded-full'/>
      {/*<span className='bg-black relative inline-flex h-2 w-2 rounded-full'/>*/}
      </span>
    </div>
          <p>alma</p>
        </div>
          </>
  )
}

export default HeroSection;
