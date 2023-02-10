/*
Two arrays are called similar if one can be obtained from another by swapping at most one pair of elements in one of the arrays.

Given two arrays a and b, check whether they are similar.

Example

For a = [1, 2, 3] and b = [1, 2, 3], the output should be
solution(a, b) = true.

The arrays are equal, no need to swap any elements.

For a = [1, 2, 3] and b = [2, 1, 3], the output should be
solution(a, b) = true.

We can obtain b from a by swapping 2 and 1 in b.

For a = [1, 2, 2] and b = [2, 1, 1], the output should be
solution(a, b) = false.

Any swap of any two elements either in a or in b won't make a and b equal.


 */
object AreSimilarSolution extends App {

  def solution(a: Array[Int], b: Array[Int]): Boolean = {
    if (a.sameElements(b)) true
    else {
      val aWithIndex = a.zipWithIndex
      val bWithIndex = b.zipWithIndex
      val zippedArrays = aWithIndex.zip(bWithIndex)

      val result = for {
        elements <- zippedArrays
        if elements._1._1 != elements._2._1
      } yield (elements._1, elements._2)
      println(result.mkString("Array(", ", ", ")"))
      if (result.length > 2) false
      else
        {
          if (result.length == 2) {
            println(aWithIndex.mkString("Array(", ", ", ")"))
            val aSwaped = aWithIndex.map {
              case (_,y) if y == result(0)._1._2 => (result(1)._1._1, y)
              case (_,y) if y == result(1)._1._2 => (result(0)._1._1, y)
              case (x,y) => (x,y)
            }.map(_._1)
            if (aSwaped.sameElements(b))
              true
            else
              false
          }
          else
            false

        }
    }
  }

  println(solution(Array(1,2,1,2), Array(2,2,1,1)))

}
