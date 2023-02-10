/*
Given an integer array arr and an integer k, modify the array by repeating it k times.

For example, if arr = [1, 2] and k = 3 then the modified array will be [1, 2, 1, 2, 1, 2].

Return the maximum sub-array sum in the modified array. Note that the length of the sub-array can be 0 and its sum in that case is 0.

As the answer can be very large, return the answer modulo 109 + 7.
 */

object KConcatMaxSumSolution extends App {

  def kConcatenationMaxSum(arr: Array[Int], k: Int): Int = {
    arr.length match {
      case 0 => 0
      case _ =>
        arr.sum * k
    }
  }


  println(kConcatenationMaxSum(Array(), 5))

}
