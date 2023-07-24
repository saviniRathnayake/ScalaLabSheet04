
object Tutorial06 {
  def caesarEncrypt(plaintext: String, shift: Int): String = {
    val encryptedText = plaintext.map { char =>
      if (char.isLetter) {
        val isUppercase = char.isUpper
        val normalizedChar = char.toUpper
        val encryptedChar = ((normalizedChar - 'A' + shift) % 26 + 'A').toChar
        if (isUppercase) encryptedChar else encryptedChar.toLower
      } else {
        char
      }
    }
    encryptedText.mkString
  }

  def caesarDecrypt(ciphertext: String, shift: Int): String = {
    val decryptedText = ciphertext.map { char =>
      if (char.isLetter) {
        val isUppercase = char.isUpper
        val normalizedChar = char.toUpper
        val decryptedChar = ((normalizedChar - 'A' - shift + 26) % 26 + 'A').toChar
        if (isUppercase) decryptedChar else decryptedChar.toLower
      } else {
        char
      }
    }
    decryptedText.mkString
  }

  def main(args: Array[String]): Unit = {
    val plaintext = "Hello, Caesar!"
    val shift = 3

    // Encryption
    val encryptedText = caesarEncrypt(plaintext, shift)
    println(s"Encrypted: $encryptedText")

    // Decryption
    val decryptedText = caesarDecrypt(encryptedText, shift)
    println(s"Decrypted: $decryptedText")
  }
}
