module Ex1 where 

import Hex

-- actual assignment
cipherText = hexByteStringToInt "09e1c5f70a65ac519458e7e53f36"
message = stringToAscii "attack at dawn"

key = listXor cipherText message

newMessage = stringToAscii "attack at dusk"
newCipherText = listXor key newMessage

ctHex = writeIntListToHexString newCipherText