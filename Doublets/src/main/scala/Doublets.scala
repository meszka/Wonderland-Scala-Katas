import scala.io.Source

object Doublets {
  val dictionary = Source.fromResource("words.txt").getLines.toSeq

  def distance(word1: String, word2: String): Int =
    (word1, word2).zipped.map(_ == _).count(_ == false)

  def neighboors(word: String): Seq[String] =
    dictionary.filter(_.length == word.length).filter(distance(_, word) == 1)

  def bfs(start: String, target: String): List[String] = {
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

  def main(args: Array[String]): Unit = println(dictionary)
}
