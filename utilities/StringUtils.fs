module StringUtils

let concatWithDelimiter delimiter str1 str2 =
    str1 + delimiter + str2

let concatWithNewline = concatWithDelimiter "\n"

let concatWithComma = concatWithDelimiter ","
    
let concatWithSpacedPipe = concatWithDelimiter " | "