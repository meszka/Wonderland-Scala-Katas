object AlphabetCipher {
  def extendToLength(s: String, n: Int): String =
    Stream.continually(s).flatten.take(n).mkString

  def prefixes(s: String): Iterator[String] =
    (1 to s.length).toIterator.map(n => s.substring(0, n))

  def shortestWord(s: String): String =
    prefixes(s).find(word => extendToLength(word, s.length) == s).get

  def addChars(k: Char, m: Char): Char = {
    val i = m - 'a'
    val j = k - 'a'
    val l = (i + j) % 26
    ('a' + l).toChar
  }

  def subChars(k: Char, m: Char): Char = {
    val i = m - 'a'
    val j = k - 'a'
    val l = (i - j + 26) % 26
    ('a' + l).toChar
  }

  def encode(keyword: String, message: String): String = {
    val extKeyword = extendToLength(keyword, message.length)
    (extKeyword, message).zipped.map(addChars).mkString
  }

  def decode(keyword: String, message: String): String = {
    val extKeyword = extendToLength(keyword, message.length)
    (extKeyword, message).zipped.map(subChars).mkString
  }

  def decipher(cipher: String, message: String): String = {
    val loopedKeyword = (message, cipher).zipped.map(subChars).mkString
    shortestWord(loopedKeyword)
  }
}
