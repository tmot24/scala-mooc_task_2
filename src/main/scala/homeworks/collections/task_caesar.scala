package homeworks.collections

object task_caesar {

  /**
   * В данном задании Вам предлагается реализовать функции,
   * реализующие кодирование/декодирование строки шифром Цезаря.
   * https://ru.wikipedia.org/wiki/Шифр_Цезаря
   * Алфавит - прописные латинские буквы от A до Z.
   * Сдвиг   - неотрицательное целое число.
   * Пример: при сдвиге 2 слово "SCALA" шифруется как "UECNC".
   */
  /**
   * @param word   входное слово, которое необходимо зашифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return зашифрованное слово
   */
  def encrypt(word: String, offset: Int): String = {

    /**
     * Решил двумя разными способами, реализация encrypt мне нравиться больше чем decrypt, что скажите?
    на всякий случай вставлю ещё и реализацию decrypt ниже альтернативным способом
     */

    word
      .toCharArray
      .map(_.toInt)
      .map(_ + offset % 26)
      .collect {
        case i if i > 90 => i - 26
        case i => i
      }
      .map(_.toChar)
      .mkString("")
  }


  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String = {

    val alphabet = 'A' to 'Z'
    val charArray = cipher.toCharArray

    val arrayInt = for {
      c <- charArray
    } yield if (alphabet.indexOf(c) > 0) {
      (alphabet.indexOf(c) - offset % 26) % 26
    } else {
      (26 + alphabet.indexOf(c) - offset % 26) % 26
    }

    val arrayChar = for {
      i <- arrayInt
    } yield alphabet(i)

    arrayChar.mkString("")

    /**
     * word
        .toCharArray
        .map(_.toInt)
        .map(_ - offset % 26)
        .collect {
          case i if i < 65 => i + 26
          case i => i
        }
        .map(_.toChar)
        .mkString("")
     */
  }

}
