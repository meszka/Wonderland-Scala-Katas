import scala.io.Source

object Doublets {
  val fullDictionary = Source.fromResource("words_big.txt").getLines.toSeq

  def distance(word1: String, word2: String): Int =
    (word1, word2).zipped.map(_ == _).count(_ == false)

  def bfs(start: String, target: String): List[String] = {
    val dictionary = fullDictionary.filter(_.length == start.length)
    def neighboors(word: String): Seq[String] =
      dictionary.filter(distance(_, word) == 1)
    def aux(queue: List[List[String]]): List[String] = {
      if (queue.isEmpty)
        List()
      else {
        val path = queue.head
        val current  = path.head
        if (current == target)
          path.reverse
        else
          aux(queue.tail ++ neighboors(current).map(_ :: path))
      }
    }
    aux(List(List(start)))
  }

  def doublets(word1: String, word2: String): Seq[String] =
    bfs(word1, word2)

  def main(args: Array[String]): Unit = println(fullDictionary)
}
