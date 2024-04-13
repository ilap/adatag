export const hexToASCII = hex => {
  return hex
    .match(/.{1,2}/g)
    .map(byte => String.fromCharCode(parseInt(byte, 16)))
    .join('')
}

export function stringifyData(data: unknown) {
  return JSON.stringify(
    data,
    (key, value) => (typeof value === 'bigint' ? value.toString() : value),
    '  '
  )
}
