import { extendVariants, Button as NextUIButton } from '@nextui-org/react'

export const Button = extendVariants(NextUIButton, {
  variants: {
    isDisabled: {
      true: 'bg-[#eaeaea] text-[#8e8e8e]  opacity-100 cursor-not-allowed',
    },
    size: {
      xs: 'px-2 min-w-12 h-6 text-tiny gap-1 rounded-full',
      sm: "px-3 min-w-16 h-8 text-tiny gap-2 rounded-full",
      md: "px-4 min-w-20 h-10 text-small gap-2 rounded-full",
      lg: "px-6 min-w-24 h-12 text-medium gap-3 rounded-full",
      xl: 'px-8 min-w-28 h-14 text-xl gap-4 rounded-full',
      xxl: 'px-10 min-w-36 h-18 text-xl gap-8 rounded-full',
      xxxl: 'px-12 min-w-44 h-24 text-2xl gap-10 rounded-full',
    },
  },
  compoundVariants: [ // <- modify/add compound variants
  {
    //isDisabled: true,
    //color: "orange",
    //class: "bg-[#84cc16]/80 opacity-100",
  },],
  defaultVariants: {
    // <- modify/add default variants
    color: 'primary',
    vairant: 'solid',
    //size: 'lg',
  },
})
