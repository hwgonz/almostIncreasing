object CodeBots extends App {

  def matrixElementsSum(matrix: Array[Array[Int]]): Int = {

    def getSumOnlyPositives(aVector: Vector[Int]): Int = {
      val onlyPositives = aVector.takeWhile(elem => elem != 0)
      val result = onlyPositives.sum
      result
    }

    val vectorMatrix = matrix.toVector
    val vector = for {
      internal <- vectorMatrix
    } yield internal.toVector
    vector.foreach(println)

    val invertedMatrix = vector.transpose
    invertedMatrix.foreach(println)

    val result = for {
      vector <- invertedMatrix
      item = getSumOnlyPositives(vector)
    } yield item
    val answer = result.sum
    println(s" The result is: $answer")

    answer
  }

  val testArr: Array[Array[Int]] = Array( Array(1, 1, 1, 0), Array(0, 5, 0, 1), Array(2, 1, 3, 10))
  println(matrixElementsSum(testArr))

}
