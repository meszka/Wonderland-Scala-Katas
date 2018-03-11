object Table {
  // I abandoned this path but the code is kind of neat so I'm keeping it here

  val alphabet = ('a' to 'z').mkString

  def rotate(s: String): String = s.tail :+ s.head

  val table = Iterator.iterate(alphabet)(rotate).take(26).toList
}
