import React, { ReactNode } from 'react'

import {
  description as disclaimerDescr,
  data as disclaimerData,
} from './data/disclaimer_data'
import { description as termsDescr, data as termsData } from './data/terms_data'
import {
  description as policyDescr,
  data as policyData,
} from './data/policy_data'

//import { GenericLegalPage } from './GenericLegalPage';

//import React, { ReactNode } from 'react'

import { AnimatePresence, motion } from 'framer-motion'
import { heroAnimation } from '../Hero/constants'

//import { LegalCard } from './LegalCard.tsx'

interface LegalPageProps {
  type: 'terms' | 'policy' | 'disclaimer'
}

export const LegalPage: React.FC<LegalPageProps> = ({ type }) => {
  const dataMap = {
    terms: { data: termsData, description: termsDescr },
    disclaimer: { data: disclaimerData, description: disclaimerDescr },
    policy: { data: policyData, description: policyDescr },
  }

  const { data, description } = dataMap[type]

  return <GenericLegalPage headerDescr={description} data={data} />
}

interface GenericLegalProps {
  headerDescr: {
    title: string
    subtitle: string
  }
  data: {
    title: string
    content: JSX.Element
  }[]
}

export const GenericLegalPage: React.FC<GenericLegalProps> = ({
  headerDescr,
  data,
}) => {
  return (
    <>
      <AnimatePresence>
        <motion.section className="flex flex-col items-center justify-center h-screen -m-16">
          <div className={`legal flex items-center justify-center mx-auto`}>
            <motion.div
              className={`text-center'}`}
              variants={heroAnimation}
              initial="initial"
              animate="animate"
            >
              <h1 className="text-8xl font-black mb-16 tracking-tighter">
                {headerDescr.title}
              </h1>
              <h2 className="text-2xl font-medium tracking-tighter">
                {headerDescr.subtitle}
              </h2>
            </motion.div>
          </div>
        </motion.section>
      </AnimatePresence>
      <section className="bg-background py-20  rounded-b-[72px] px-8-">
        <div className="ms-6 me-6">
          {data.map((item, index) => (
            <LegalCard key={index} title={item.title} content={item.content} />
          ))}
        </div>
      </section>
    </>
  )
}

interface Props {
  title: string
  content: ReactNode
}

const LegalCard: React.FC<Props> = ({ title, content }) => {
  return (
    <div className="legal-card flex justify-center border border-gray-300 mt-6 mb-6 rounded-[56px] w-full">
      <div className="max-w-[1000px] mx-auto grid grid-cols-12 gap-6 my-24">
        <div className="text-4xl font-semibold tracking-tighter col-span-12 md:col-span-4">
          <h2>{title}</h2>
        </div>
        <div className="text-2xl font-medium tracking-tighter col-span-12 md:col-span-8">
          {content}
        </div>
      </div>
    </div>
  )
}
