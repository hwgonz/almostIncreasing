import io.StdIn.readLine
object SuperDigitSolution extends App {
  def superDigit(x: String): String =
    if (x.length == 1) x else superDigit(x.foldLeft(0)(_ + _.asDigit).toString)

  val nk = readLine.split(" ")
  println(superDigit(superDigit(nk(0)) * nk(1).toInt))


}
