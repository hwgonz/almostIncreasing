
object AlmostIncreasing extends App {


  def almostIncreasingSequence(sequence: Array[Int]): Boolean = {

    def getBreakingElements(aVector: Vector[Int]): Vector[Vector[(Int, Int)]] = {
      val asSlides = aVector
        .zipWithIndex
        .sliding(2)
        .toVector
      val target = asSlides.filter(item => item.head._1 >= item.last._1)
      target
    }

    def evalSide(breakPoint: Int, aVector: Vector[Int]): Vector[Vector[(Int, Int)]] = {
      val testVector = aVector
        .zipWithIndex
        .filter(_._2 != breakPoint)
        .map(_._1)

      val target = getBreakingElements(testVector)
      target
    }

    val aVector: Vector[Int] = sequence.toVector
    if (aVector.length <= 1) true
    else {
      val target = getBreakingElements(aVector)
      if (target.nonEmpty && aVector.size > 2) {
        val breakPointRight = target.head.last._2
        val breakPointLeft = target.head.head._2

        val targetRight = evalSide(breakPointRight, aVector)

        val targetLeft = evalSide(breakPointLeft, aVector)

        if (targetRight.nonEmpty && targetLeft.nonEmpty) false
        else true
      }
      else true
    }
  }

  val testSeq: Array[Int] = Array( 1, 1)
  println(almostIncreasingSequence(testSeq))

}
