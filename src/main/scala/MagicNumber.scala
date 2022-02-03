import scala.annotation.tailrec

object MagicNumber extends App {
  @tailrec
  def magicNumber(number: Int): Int = {
    if (number < 10)
      number
    else
      magicNumber(number % 10 + number / 10)
  }

  println(magicNumber(9875))
  println(magicNumber(50311))
}
