interface CloseIconProps {
  width: number;
  height: number;
}

export const CloseIcon = ({ width, height }: CloseIconProps) => (
  <svg
    width={width}
    height={height}
    viewBox="0 0 24 24"
    fill="currentColor"
    role="img"
    aria-label="close-icon"
    className="close-icon"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      id="Vector"
      d="M16 16L12 12M12 12L8 8M12 12L16 8M12 12L8 16"
      stroke="currentColor"
      strokeWidth="2"
      strokeLinecap="round"
      strokeLinejoin="round"
    />
  </svg>
);