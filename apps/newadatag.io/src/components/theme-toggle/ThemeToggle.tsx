import { useEffect, useState } from 'react'
import { SunAndMoon } from './SunAndMoon'
import './ThemeToggle.css'

const ThemeToggle = () => {
  const [selectedTheme, setSelectedTheme] = useState(
    () => localStorage.getItem('selectedTheme') || ''
  )

  useEffect(() => {
    const prefersDark = window.matchMedia('(prefers-color-scheme: dark)')
    const prefersLight = window.matchMedia('(prefers-color-scheme: light)')

    const updateTheme = () => {
      const storedTheme = localStorage.getItem('selectedTheme')
      const setTheme = (theme: string) => {
        document.documentElement.setAttribute('data-theme', theme)
        setSelectedTheme(theme)
      }

      if (storedTheme !== null) {
        setTheme(storedTheme)
      } else {
        switch (true) {
          case prefersDark.matches:
            setTheme('dark')
            break
          case prefersLight.matches:
            setTheme('light')
            break
          default:
            break
        }
      }
    }

    updateTheme()

    prefersDark.addEventListener('change', updateTheme)
    prefersLight.addEventListener('change', updateTheme)

    return () => {
      prefersDark.removeEventListener('change', updateTheme)
      prefersLight.removeEventListener('change', updateTheme)
    }
  }, [])

  const handleThemeChange = (themeName: string) => {
    setSelectedTheme(themeName)
    document.documentElement.setAttribute('data-theme', themeName)
    localStorage.setItem('selectedTheme', themeName)
  }

  const onClick = () => {
    if (selectedTheme === 'light') {
      handleThemeChange('dark')
    } else {
      handleThemeChange('light')
    }
  }

  return (
    <>
      <span className="lg:hidden">
        <button onClick={onClick}>
          {selectedTheme === 'light' ? 'Dark' : 'Light'} theme
        </button>
      </span>
      <span className="hidden lg:block">
        <button
          type="button"
          className="theme-toggle"
          id="theme-toggle"
          title="Toggles light & dark"
          aria-label={selectedTheme}
          aria-live="polite"
          onClick={onClick}
        >
          <SunAndMoon />
        </button>
      </span>
    </>
  )
}

export default ThemeToggle
