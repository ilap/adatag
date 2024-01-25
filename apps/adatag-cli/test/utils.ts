function generateRandomStrings(count: number): string[] {
    const maxLength = 15
    const a = 'a'.charCodeAt(0)
    const z = 'z'.charCodeAt(0)
  
    const randomStrings: string[] = []
  
    for (let j = 0; j < count; j++) {
      const stringLength = Math.floor(Math.random() * (maxLength + 1))
      const str: string[] = []
  
      for (let i = 0; i < stringLength; i++) {
        const r = Math.floor(Math.random() * (z - a + 1)) + a
        str.push(String.fromCharCode(r))
      }
  
      const rs = str.join('')
      if (rs === '') {
        continue
      }
      randomStrings.push(str.join(''))
    }
  
    return randomStrings
  }