import { extendVariants, Input as NextUIInput } from '@nextui-org/react'

export const Input = extendVariants(NextUIInput, {
  variants: {
    color: {
      default: {
        inputWrapper: [
          'border-black',
          // other styles...
        ],
        description: [
          'text-black',
          // other styles...
        ],
      },
      orange: {
        inputWrapper: [
          'border-orange-500',
          'data-[hover=true]:border-orange-700',
          'group-data-[focus=true]:border-orange-800',
          // other styles...
        ],
        description: [
          'text-orange-500',
          // other styles...
        ],
      },
    },
    size: {
      xs: {
        inputWrapper: 'h-unit-6 min-h-unit-6 px-1',
        input: 'text-lg',
      },
      md: {
        inputWrapper: 'h-unit-10 min-h-unit-10',
        input: 'text-xl',
      },
      xl: {
        inputWrapper: 'h-unit-18 min-h-unit-18',
        input: 'text-2xl',
        description: 'text-small',
        errorMessage: 'text-small'
      },
    },
    textSize: {
      large: {
        input: 'text-2xl',
      },
    },
    removeLabel: {
      true: {
        label: 'hidden',
      },
      false: {},
    },
  },
  defaultVariants: {
    textSize: 'large',
    removeLabel: true,
  },
})
