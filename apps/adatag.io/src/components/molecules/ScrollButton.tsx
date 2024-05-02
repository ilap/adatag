import React from 'react'
import { Button } from '../atoms/Button.tsx'
import { motion, useScroll, useTransform } from 'framer-motion'
import { ArrowDownIcon } from '@heroicons/react/24/outline'

interface Props {
  onClick: () => void
}

export const ScrollButton: React.FC<Props> = ({ onClick }) => {
  const { scrollYProgress } = useScroll()
  const invertedScrollYProgress = useTransform(scrollYProgress, [0, 0.04], [1, 0])

  return (
    <motion.div
      style={{ opacity: invertedScrollYProgress }}
      initial={{ opacity: 0 }}
      animate={{ opacity: 1 }}
      exit={{ opacity: 0 }}
      transition={{ duration: 0.5 }}
      className="fixed bottom-10 left-10">
      <Button
        isIconOnly
        aria-label="Like"
        variant="bordered"
        radius="full"
        className="h-16 w-16 text-foreground border-foreground border-1"
        onClick={onClick}>
        <ArrowDownIcon className="w-4 h4" />
      </Button>
    </motion.div>
  )
}
