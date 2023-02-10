/*
Given an array of integers, find the maximal absolute difference between any two of its adjacent elements.

Example

For inputArray = [2, 4, 1, 0], the output should be
solution(inputArray) = 3.
 */
object ArrayMaximalAdjacentDifferenceSolution extends App {

  def adjacentDifference(inputList: List[Int], maximum: Int): Int = {
    inputList match {
      case first :: second :: Nil =>  if ((first - second).abs > maximum) (first - second).abs
                                      else maximum
      case first :: second :: rest => if ((first - second).abs > maximum) adjacentDifference(second +: rest, (first - second).abs)
                                      else adjacentDifference(second +: rest, maximum)
    }
  }

  def solution(inputArray: Array[Int]): Int = {
    adjacentDifference(inputArray.toList, 0)
  }

  println(solution(Array(2,4,1,0)))
}
