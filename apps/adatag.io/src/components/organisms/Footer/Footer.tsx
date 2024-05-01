import React from 'react'
import { Link } from '@nextui-org/react'
import { DiscordIcon } from '../../../assets/DiscordIcon'
import { GithubIcon } from '../../../assets/GithubIcon'
import { Logo } from '../../../assets/Logo'

export const Footer: React.FC = () => (
  <footer className="bg-foreground text-white py-12 rounded-t-[72px] pt-32 pb-32 relative">
    <div className="absolute -top-20 left-0 right-0 bottom-0 z-[-1] bg-foreground" />
    <div className="container lg:max-w-4xl xl:max-w-5xl flex mx-auto flex-wrap justify-between items-start">
      <div className="flex w-full mb-6">
        <Logo opacity={0.75} width={64} height={64} />
      </div>
      <div className="font-medium tracking-tighter text-3xl w-full mt-2 sm:w-1/2 md:w-1/2 lg:w-1/2">
        Make your mark with <br /> adatag
      </div>

      <div className="flex flex-col w-full sm:w-1/4 md:w-1/4 lg:w-1/4 mt-4 sm:mt-0">
        <Link
          className="text-xl text-gray-50 py-2"
          size="lg"
          href="/"
          underline="hover"
        >
          Home
        </Link>
        <Link
          className="text-xl text-gray-50 py-2"
          size="lg"
          href="/faq"
          underline="hover"
        >
          FAQs
        </Link>
      </div>

      <div className="flex flex-col w-full sm:w-1/4 md:w-1/4 lg:w-1/4 mt-4 sm:mt-0">
        <Link
          className="text-xl text-gray-50 py-2"
          size="lg"
          href="/terms"
          underline="hover"
        >
          Terms & Conditions
        </Link>
        <Link
          className="text-xl text-gray-50 py-2"
          size="lg"
          href="/policy"
          underline="hover"
        >
          Privacy Policy
        </Link>
        <Link
          className="text-xl text-gray-50 py-2"
          size="lg"
          href="/disclaimer"
          underline="hover"
        >
          Disclaimer
        </Link>
      </div>

      <div className="flex w-full mt-16">
        <Link
          className="mr-6 font-normal text-gray-50"
          size="sm"
          href="https://github.com/ilap/adatag"
          isExternal
          underline="none"
        >
          <GithubIcon width={24} height={24} />
        </Link>
        <Link
          className="mr-6 font-normal text-gray-50"
          size="sm"
          //** TODO: add proper discord channel.
          href="https://discord.com/ilap"
          isExternal
          underline="none"
        >
          <DiscordIcon width={24} height={24} />
        </Link>
        <Link
          //** TODO: add proper email.
          className="mr-6 font-normal text-gray-50"
          size="md"
          href="mailto:hello@adatag.io"
          isExternal
          underline="always"
        >
          hello@adatag.io
        </Link>
      </div>
    </div>
  </footer>
)
