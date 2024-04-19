import React, { useRef } from 'react'
import { faqData } from './data.tsx'
import { AnimatePresence, motion, useScroll, useTransform } from 'framer-motion'
import { heroAnimation } from '../Hero/constants.ts'
import { Accordion } from '../../atoms/Accordion.tsx'
import { ArrowDownIcon } from '@heroicons/react/24/outline'
import { Button } from '../../atoms/Button.tsx'

export const FAQ: React.FC = () => {
    const accordionRef = useRef<HTMLDivElement>(null);
    const { scrollYProgress } = useScroll();
    const invertedScrollYProgress = useTransform(scrollYProgress, [0, 0.04], [1, 0])

    const scrollToAccordion = () => {
        if (accordionRef.current) {
          const navbarHeight = 92;
          const targetPosition = accordionRef.current.offsetTop - navbarHeight;
          window.scrollTo({ top: targetPosition, behavior: 'smooth' });
        }
    };
      
    

  return (
    <>
      <AnimatePresence>
        <motion.section
          className="flex items-center h-screen"
          variants={heroAnimation}
          initial="initial"
          animate="animate"
        >
          <div
            className={`flex items-center justify-center min-w-[1152px] mx-auto`}
          >
            <div className={`text-center'}`}>
              <h1 className="text-8xl text-center font-black mb-16 tracking-tighter">
                Frequently
                <br />
                Asked
                <br />
                Questions
              </h1>
            </div>
          </div>
          <motion.div
          style={{ opacity: invertedScrollYProgress }}
          initial={{ opacity: 0  }}
          animate={{ opacity: 1 }}
          exit={{ opacity: 0 }}
          transition={{ duration: 0.5 }}
          className="fixed bottom-8 left-1/2 transform -translate-x-1/2 "
        >
          <Button
            isIconOnly
            aria-label="Like"
            variant="bordered"
            //size="xl"
            radius="full"
            //color="primary"
            className="h-16 w-16 text-foreground border-foreground border-1"
            onClick={scrollToAccordion}
          >
            <ArrowDownIcon className='w-4 h4'/>
          </Button>
          </motion.div>
        </motion.section>
      </AnimatePresence>
      <div className=" mx-6 accordion" ref={accordionRef}>
        {faqData.map(item => (
          <Accordion
            key={item.question}
            title={item.question}
            content={item.answer}
          />
        ))}
      </div>
    </>
  )
}
