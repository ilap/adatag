export type MintDetailsDialogProps = {
  isOpen: boolean;
  onClose: () => void;
  onMint: () => void;
  adatag: string;
  deposit: string;
  handleOpenProgressDialog: () => void;
};

// ... (import statements)

type Rarity = {
  name: string;
  color: string;
  description: string;
};

const rarityData: { [key: number]: Rarity } = {
  1: {
    name: 'Legendary',
    color: '#ff0006',
    description: 'Extremely rare and valuable item.',
  },
  2: {
    name: 'Ultra Rare',
    color: '#ea00ff',
    description: 'Highly rare and valuable item.',
  },
  3: {
    name: 'Rare',
    color: '#005ecc',
    description: 'Rare and valuable item.',
  },
  4: {
    name: 'Common',
    color: '#449900',
    description: 'Common item with some value.',
  },
  8: {
    name: 'Basic',
    color: '#ff6600',
    description: 'Basic item with little value.',
  },
};

export const getRarity = (length: number): Rarity => {
  console.log(`LENGTH ${length}`)
  if (length === 1) {
    return rarityData[1];
  } else if (length === 2) {
    return rarityData[2];
  } else if (length === 3) {
    return rarityData[3];
  } else if (length >= 4 && length <= 7) {
    return rarityData[4];
  } else if (length >= 8 && length <= 16) {
    return rarityData[8];
  } else {
    return {
      name: 'Unknown',
      color: '#ff0006',
      description: 'Rarity cannot be determined.',
    };
  }
};

// ... (existing code)
