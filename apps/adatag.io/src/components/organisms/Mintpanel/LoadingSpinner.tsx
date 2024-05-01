import { CircularProgress } from '@nextui-org/react'

interface Props {
  isProgressing: boolean
  content: JSX.Element
}

export const LoadingSpinner: React.FC<Props> = ({ isProgressing, content }) =>
  isProgressing && (
    <div className="absolute inset-0 p-4 flex flex-col items-center z-10 justify-center bg-white bg-opacity-75 backdrop-blur-sm">
      <CircularProgress
        aria-label="Loading Spinner"
        size="lg"
        className="p-5"
      />
      <div className="font-bold text-xl p-5">{content}</div>
    </div>
  )
