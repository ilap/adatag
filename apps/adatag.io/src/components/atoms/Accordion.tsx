import React, { useState } from 'react';
import { motion } from 'framer-motion';
import { MinusIcon, PlusIcon } from '@heroicons/react/24/outline'

export const Accordion = ({ title, content }) => {
  const [isActive, setIsActive] = useState(false);

  const variants = {
    open: {
      opacity: 1,
      transition: { duration: 1.0 },
    },
    closed: {
      opacity: 0,
      transition: { duration: 1.0 },
    },
  };

  const toggleVariants = {
    open: { rotate: 0 },
    closed: { rotate: -90 },
  };

  return (
    <div className="flex justify-center border border-gray-300 m-12 rounded-[56px] w-full mx-auto">
      <div className="w-full m-12 lg:max-w-[1152px] md:max-w-[800px] sm:max-w-[600px]">
        <div className="flex text-left items-center mx-[-64px] p-4 cursor-pointer" onClick={() => setIsActive(!isActive)}>
        <motion.div
            animate={isActive ? "open" : "closed"}
            variants={toggleVariants}
            transition={{ duration: 0.3 }}
            className="text-4xl font-medium"
          >
            {isActive ? <MinusIcon className=" h-8 w-8 cursor-pointer" />: <PlusIcon className="h-8 w-8 cursor-pointer"/>}
          </motion.div>
          <div className="text-4xl  mx-12 font-medium">{title}</div>

        </div>
        {isActive && (
          <motion.div
            initial="closed"
            animate="open"
            variants={variants}
            className="p-6 w-full text-3xl text-left  overflow-hidden"
          >
            {content}
          </motion.div>
        )}
        </div>
    </div>
  );
};

export default Accordion;
