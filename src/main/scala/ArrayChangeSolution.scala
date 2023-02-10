/*
You are given an array of integers. On each move you are allowed to increase exactly one of its element by one. Find the minimal number of moves required to obtain a strictly increasing sequence from the input.

Example

For inputArray = [1, 1, 1], the output should be
solution(inputArray) = 3.
 */

object ArrayChangeSolution extends App {

  def processArray(inputList: List[Int], moves: Int): Int = {
    inputList match {
      case first :: second :: Nil if second > first => moves
      case first :: second :: Nil if second == first => moves + 1
      case first :: second :: Nil if second < first => moves + (first - second) + 1
      case first :: second :: rest if second > first => processArray((second +: rest), moves)
      case first :: second :: rest if second == first => processArray(((second + 1) +: rest), moves + 1)
      case first :: second :: rest if second < first => processArray(((first + 1) +: rest), moves + (first - second) + 1)
    }
  }

  def solution(inputArray: Array[Int]): Int = {
    processArray(inputArray.toList, 0)
  }

  println(solution(Array(-1000, 0, -2, 0)))

}
