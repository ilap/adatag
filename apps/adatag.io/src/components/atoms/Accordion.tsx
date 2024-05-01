import React, { useState } from 'react'
import { motion } from 'framer-motion'
import { MinusIcon, PlusIcon } from '@heroicons/react/24/outline'
import { toggleVariants, accordionVariants } from './constants'

interface Props {
  title: string
  content: JSX.Element
}

export const Accordion: React.FC<Props> = ({ title, content }) => {
  const [isActive, setIsActive] = useState(false)

  return (
    <div className="flex justify-center border border-gray-300 mt-6 mb-6 rounded-[56px] w-full">
      <div className="w-full m-12 lg:max-w-[860px] md:max-w-[640px] sm:max-w-[400px]">
        <div
          className="flex text-left items-center -mx-16 p-4 cursor-pointer"
          onClick={() => setIsActive(!isActive)}
        >
          <motion.div
            animate={isActive ? 'open' : 'closed'}
            variants={toggleVariants}
            transition={{ duration: 0.3 }}
            className="text-4xl font-medium"
          >
            {isActive ? (
              <MinusIcon className=" h-8 w-8 cursor-pointer" />
            ) : (
              <PlusIcon className="h-8 w-8 cursor-pointer" />
            )}
          </motion.div>
          <div className="text-4xl  mx-10 font-medium tracking-tighter">
            {title}
          </div>
        </div>
        {isActive && (
          <motion.div
            initial="closed"
            animate="open"
            variants={accordionVariants}
            className="p-6 w-full text-3xl text-left  overflow-hidden tracking-tighter"
          >
            {content}
          </motion.div>
        )}
      </div>
    </div>
  )
}
