import React, { useRef } from 'react'
import { faqData } from './data.tsx'
import { AnimatePresence, motion } from 'framer-motion'
import { heroAnimation } from '../Hero/constants'
import { Accordion } from '../../atoms/Accordion.tsx'
import { ScrollButton } from '../../molecules/ScrollButton.tsx'

export const FAQ: React.FC = () => {
  const accordionRef = useRef<HTMLDivElement>(null)

  const scrollToAccordion = () => {
    if (accordionRef.current) {
      const navbarHeight = 92
      const targetPosition = accordionRef.current.offsetTop - navbarHeight
      window.scrollTo({ top: targetPosition, behavior: 'smooth' })
    }
  }

  return (
    <>
      <AnimatePresence>
        <motion.section className="flex flex-col items-center justify-center h-screen ">
          <div className={`flex items-center justify-center mx-auto`}>
            <motion.div
              className={`text-center'}`}
              variants={heroAnimation}
              initial="initial"
              animate="animate"
            >
              <h1 className="text-8xl text-center font-black mb-16 tracking-tighter">
                Frequently
                <br />
                Asked
                <br />
                Questions
              </h1>
            </motion.div>
          </div>
          <ScrollButton onClick={scrollToAccordion} />
        </motion.section>
      </AnimatePresence>
      <section
        className="bg-background py-20  rounded-b-[72px] px-8-"
        ref={accordionRef}
      >
        <div className="ms-6 me-6">
          {faqData.map(item => (
            <Accordion
              key={item.question}
              title={item.question}
              content={<>{item.answer}</>}
            />
          ))}
        </div>
      </section>
    </>
  )
}
