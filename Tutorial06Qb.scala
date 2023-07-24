object Tutorial06Qb {
  def caesarEncrypt(plaintext: String, shift: Int): String = {
    plaintext.map { char =>
      if (char.isLetter) {
        val isUppercase = char.isUpper
        val normalizedChar = char.toUpper
        val encryptedChar = ((normalizedChar - 'A' + shift) % 26 + 'A').toChar
        if (isUppercase) encryptedChar else encryptedChar.toLower
      } else {
        char
      }
    }.mkString
  }

  def caesarDecrypt(ciphertext: String, shift: Int): String = {
    ciphertext.map { char =>
      if (char.isLetter) {
        val isUppercase = char.isUpper
        val normalizedChar = char.toUpper
        val decryptedChar = ((normalizedChar - 'A' - shift + 26) % 26 + 'A').toChar
        if (isUppercase) decryptedChar else decryptedChar.toLower
      } else {
        char
      }
    }.mkString
  }

  def cipher(data: String, process: (String, Int) => String, shift: Int): String = {
    process(data, shift)
  }

  def main(args: Array[String]): Unit = {
    val plaintext = "Hello, Caesar!"
    val shift = 3

    // Using the cipher function with encryption
    val encryptedText = cipher(plaintext, caesarEncrypt, shift)
    println(s"Encrypted: $encryptedText")

    // Using the cipher function with decryption
    val decryptedText = cipher(encryptedText, caesarDecrypt, shift)
    println(s"Decrypted: $decryptedText")
  }
}
