/*
Several people are standing in a row and need to be divided into two teams. The first person goes into team 1, the second goes into team 2, the third goes into team 1 again, the fourth into team 2, and so on.

You are given an array of positive integers - the weights of the people. Return an array of two integers, where the first element is the total weight of team 1, and the second element is the total weight of team 2 after the division is complete.
 */

object AlternatingSumsSolution extends App {

  def solution(a: Array[Int]): Array[Int] = {
    val result = a.zipWithIndex.foldLeft(Array(0,0)) { (acc, elem) =>
      if (elem._2 % 2 == 0) Array(acc.head, acc.last + elem._1)
      else Array(acc.head + elem._1, acc.last)
    }
    result.reverse
  }

  println(solution(Array(50, 60, 60, 45, 70)).mkString("Array(", ", ", ")"))

}
