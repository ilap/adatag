// FIXME: these exports should be used from the @adatag package
export function isValidUsername(str: string): boolean {
  const n = str.length

  return (
    n > 0 &&
    n <= 16 &&
    isLowerCase(str.charCodeAt(0)) &&
    isLowerCaseOrDigit(str.charCodeAt(n - 1)) &&
    hasOnlyAllowedChars(str)
  )
}

export function isLowerCase(ch: number): boolean {
  return ch >= 97 && ch <= 122
}

function isDigit(digit: number): boolean {
  return digit >= 48 && digit <= 57
}

function isLowerCaseOrDigit(ch: number): boolean {
  return isLowerCase(ch) || isDigit(ch)
}

function isHyphen(char: number): boolean {
  return char === 45
}

function isUnderscore(char: number): boolean {
  return char === 95
}

function isDot(char: number): boolean {
  return char === 46
}

function isValidChar(ch: number): boolean {
  return (
    isLowerCase(ch) ||
    isDigit(ch) ||
    isHyphen(ch) ||
    isUnderscore(ch) ||
    isDot(ch)
  )
}

function hasOnlyAllowedChars(str: string): boolean {
  const n = str.length

  function go(i: number): boolean {
    if (i >= n) {
      return true
    }
    if (!isValidChar(str.charCodeAt(i))) {
      return false
    }
    return go(i + 1)
  }

  return go(0)
}

export function a2h(str: string) {
  const arr1 = []
  for (let n = 0, l = str.length; n < l; n++) {
    const hex = Number(str.charCodeAt(n)).toString(16)
    arr1.push(hex)
  }
  return arr1.join('')
}
