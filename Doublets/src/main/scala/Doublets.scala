import scala.io.Source

object Doublets {
  val fullDictionary = Source.fromResource("words_big.txt").getLines.toSeq

  def distance(word1: String, word2: String): Int =
    (word1, word2).zipped.map(_ == _).count(_ == false)

  def bfs(start: String, target: String): List[String] = {
    val dictionary = fullDictionary.filter(_.length == start.length)
    def neighboors(word: String): Seq[String] =
      dictionary.filter(distance(_, word) == 1)
    def aux(queue: List[List[String]], seen: Set[String], steps: Int): List[String] = {
      if (queue.isEmpty)
        List()
      else {
        val path = queue.head
        val current = path.head
        if (current == target) {
          println("steps: " + steps)
          path.reverse
        }
        else {
          val unseenNeighboors = neighboors(current).filterNot(seen.contains)
          aux(queue.tail ++ unseenNeighboors.map(_ :: path),
              seen ++ unseenNeighboors,
              steps + 1)
        }
      }
    }
    aux(List(List(start)), Set(), 0)
  }

  def bidirectionalBFS(start: String, target: String): List[String] = {
    val dictionary = fullDictionary.filter(_.length == start.length)
    def neighboors(word: String): Seq[String] =
      dictionary.filter(distance(_, word) == 1)
    def aux(leftQueue: List[List[String]],
            rightQueue: List[List[String]],
            leftSeen: Set[String],
            rightSeen: Set[String],
            steps: Int): List[String] = {
      if (leftQueue.isEmpty || rightQueue.isEmpty)
        List()
      else {
        val leftPath = leftQueue.head
        val leftCurrent = leftPath.head
        val rightPath = rightQueue.head
        val rightCurrent = rightPath.head
        if (rightSeen.contains(leftCurrent)) {
          println("steps: " + steps)
          leftPath.tail.reverse ++ rightQueue.find(_.head == leftCurrent).get
        }
        else if (leftSeen.contains(rightCurrent)) {
          println("steps: " + steps)
          leftQueue.find(_.head == rightCurrent).get.reverse ++ rightPath.tail
        }
        else {
          val leftUnseenNeighboors = neighboors(leftCurrent).filterNot(leftSeen.contains)
          val rightUnseenNeighboors = neighboors(rightCurrent).filterNot(rightSeen.contains)
          aux(leftQueue.tail ++ leftUnseenNeighboors.map(_ :: leftPath),
              rightQueue.tail ++ rightUnseenNeighboors.map(_ :: rightPath),
              leftSeen ++ leftUnseenNeighboors,
              rightSeen ++ rightUnseenNeighboors,
              steps + 2)
        }
      }
    }
    if (start.length != target.length)
      List()
    else
      aux(List(List(start)), List(List(target)), Set(), Set(), 0)
  }

  def doublets(word1: String, word2: String): Seq[String] =
    bidirectionalBFS(word1, word2)

  def solutionIsCorrect(solution: Seq[String]) =
    (solution, solution.tail).zipped.forall(distance(_, _) == 1)

  def main(args: Array[String]): Unit = println(fullDictionary)
}
