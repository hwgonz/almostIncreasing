object IsLucky extends App {
  def isLucky(n: Int): Boolean = {
    val asString = n.toString
    val halves = asString.splitAt(asString.length/2)
    val leftHalf = halves._1.toCharArray.map(item => item.asDigit).sum
    val rightHalf = halves._2.toCharArray.map(item => item.asDigit).sum
    println(s"Left Half is $leftHalf")
    println(s"Right Half is $rightHalf")
    if(rightHalf == leftHalf) true else false
  }

  val n = 1230
  println(isLucky(n))
}
