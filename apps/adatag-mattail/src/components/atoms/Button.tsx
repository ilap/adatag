import { extendVariants, Button as NextUIButton } from '@nextui-org/react'

export const Button = extendVariants(NextUIButton, {
  variants: {
    // <- modify/add variants
    color: {
      olive: 'text-[#000] bg-[#84cc16]',
      orange: 'bg-[#ffbf04] text-[#000]',
      violet: 'bg-[#8b5cf6] text-[#fff]',
    },
    isDisabled: {
      true: 'bg-[#eaeaea] text-[#8e8e8e]  opacity-100 cursor-not-allowed',
    },
    size: {
      xs: 'px-unit-2 min-w-unit-12 h-unit-6 text-tiny gap-unit-1 rounded-full',
      sm: 'px-unit-3 min-w-unit-16 h-unit-8 text-small gap-unit-2 rounded-full',
      md: 'px-unit-4 min-w-unit-20 h-unit-12 text-medium gap-unit-2 rounded-full',
      lg: 'px-unit-6 min-w-unit-24 h-unit-12 text-medium gap-unit-2 rounded-full',
      xl: 'px-unit-8 min-w-unit-28 h-unit-14 text-xl gap-unit-4 rounded-full',
      xxl: 'px-unit-10 min-w-unit-36 h-unit-18 text-xl gap-unit-8 rounded-full',
      xxxl: 'px-unit-12 min-w-unit-44 h-unit-24 text-2xl gap-unit-10 rounded-full',
    },
  },
  defaultVariants: {
    // <- modify/add default variants
    color: 'orange',
    size: 'lg',
  },
})
