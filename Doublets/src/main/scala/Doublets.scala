import scala.io.Source

object Doublets {
  val dictionary = Source.fromResource("words.txt").getLines.toSet

  def changeLetter(word1: String, word2: String, n: Int): String = {
    word1.substring(0, n) + word2(n) + word1.substring(n + 1)
  }

  def nextLink(word1: String, word2: String): String = {
    val candidates =
      for (i <- 0 until word1.length) yield changeLetter(word1, word2, i)
    candidates.filter(_ != word1).find(word => dictionary.contains(word)).get
  }

  def doublets(word1: String, word2: String): Seq[String] = {
    ???
  }

  def main(args: Array[String]): Unit = println(dictionary)
}
