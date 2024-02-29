

export const hexToASCII = (hex) => {
    return hex.match(/.{1,2}/g)
              .map((byte) => String.fromCharCode(parseInt(byte, 16)))
              .join('');
};