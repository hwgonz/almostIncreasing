object CountChars extends App {

  println(compress("uuuu")) // should print 4u
  println(compress("aabccc")) // should print 2a1b3c
  println(compress("xxxxax")) // should print 4x1a1x
  println(compress("abababaa")) // should print 1a1b1a1b1a1b2a

  def compress(in: String): String = {

    in.foldLeft(List.empty[(Char, Int)]) { (acc, current) =>
        acc match {
          case Nil => (current, 1) :: Nil
          case ((lastChar, lastCharCount) :: xs) if lastChar == current =>
            (lastChar, lastCharCount + 1) :: xs
          case xs => (current, 1) :: xs
        }
      }
      .reverseIterator
      .map { case (a, num) => s"$num$a" }
      .foldLeft("")(_ ++ _)

  }

}
