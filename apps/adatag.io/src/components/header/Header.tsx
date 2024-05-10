import ThemeToggle from '../theme-toggle/ThemeToggle'
import { GithubLogo } from '../../assets/svg/github-logo'
import { DiscordLogo } from '../../assets/svg/discord-logo'
import { AdatagLogo } from '../../assets/svg/adatag-logo'

import { useWallet, CardanoWallet } from '@meshsdk/react'

import './Header.css'
import { useState } from 'react'

export const Header = () => {
  const { connected, wallet } = useWallet()
  const [assets, setAssets] = useState<null | any>(null)
  const [loading, setLoading] = useState<boolean>(false)
  const [selectedTheme, setSelectedTheme] = useState(
    () => localStorage.getItem('selectedTheme') || '',
  )

  const isDark = selectedTheme === 'dark'

  async function getAssets() {
    if (wallet) {
      setLoading(true)
      const _assets = await wallet.getAssets()
      setAssets(_assets)
      setLoading(false)
    }
  }

  return (
    <>
      <header className="header-container: true; home-page-header: true;">
        <div className="header-inner">
          <div className="header-logo">
            <a href="/">
              <span className="sr-only">Adatag Homepage</span>
              <AdatagLogo width={130} height={44} />
            </a>
          </div>
          <ul className="lg:grow lg:flex lg:justify-end lg:p-4 menu-toolkit">
            <li>
              <ThemeToggle />
            </li>
            <li>
              <a
                href="https://github.com/ilap/adatag.iok"
                target="_blank"
                title="GitHub"
                rel="noreferrer"
              >
                <span className="lg:hidden">GitHub</span>
                <span className="hidden lg:block">
                  <GithubLogo width={22} height={22} />
                </span>
              </a>
            </li>

            <li>
              <a
                href="https://adatag.io/chat"
                target="_blank"
                title="Discord"
                rel="noreferrer"
              >
                <span className="lg:hidden">Discord</span>
                <span className="hidden lg:block">
                  <DiscordLogo width={22} height={22} />
                </span>
              </a>
            </li>
            <li>
              <div className="custom-cardano-wallet">
                <CardanoWallet isDark={false} />
              </div>
            </li>
          </ul>
        </div>
      </header>
    </>
  )
}
