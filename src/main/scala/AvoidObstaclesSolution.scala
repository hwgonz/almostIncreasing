/*
You are given an array of integers representing coordinates of obstacles situated on a straight line.

Assume that you are jumping from the point with coordinate 0 to the right. You are allowed only to make jumps of the same length represented by some integer.

Find the minimal length of the jump enough to avoid all the obstacles.

Example

For inputArray = [5, 3, 6, 7, 9], the output should be
solution(inputArray) = 4.
 */

object AvoidObstaclesSolution extends App {


  def getMaxJump(tempMax: Int, obstacles: Array[Int]): Int =
    if (!obstacles.contains(tempMax) && !obstacles.exists(obstacle => obstacle % tempMax == 0)) tempMax
    else
      getMaxJump(tempMax + 1, obstacles)



  def solution(inputArray: Array[Int]): Int = {

    val sortedArray = inputArray.sortBy(identity)
    println(s"Sorted input array: ${sortedArray.mkString("Array(", ", ", ")")}")

    getMaxJump(1, sortedArray)

  }

  println(solution(Array(5, 3, 6, 7, 9)))

  println(solution(Array(1, 4, 10, 6, 2)))

  println(solution(Array(1000, 999)))


}
