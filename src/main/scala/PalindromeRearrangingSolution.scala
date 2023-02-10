/*
Given a string, find out if its characters can be rearranged to form a palindrome.

Example

For inputString = "aabb", the output should be
solution(inputString) = true.

adrda

We can rearrange "aabb" to make "abba", which is a palindrome.
 */

object PalindromeRearrangingSolution extends App {

  def isPalindrome(input: String): Boolean = input == input.reverse

  def countChars(inputString: String): Map[Char, Int] =
    inputString.foldLeft(Map[Char, Int]()) {
      case (acc, char) => acc + (char -> (acc.getOrElse(char, 0) + 1))
    }

  def countCharsAndBoolean(inputString: String): Map[Char, Boolean] = {
    val charsAndCount = countChars(inputString)
    val charsAndBoolean = charsAndCount.map { case (char, count) => char -> isEven(count) }
    charsAndBoolean.filter(res => res._2 == false)
  }

  def isEven(value: Int): Boolean = value % 2 == 0

  def solution(inputString: String): Boolean = {
    inputString.length match {
      case 1 => true
      case 2 => isPalindrome(inputString)
      case len if isEven(len) =>  // even case
        val result = countCharsAndBoolean(inputString)
        if (result.nonEmpty) false
        else true
      case len if !isEven(len)=> // odd case
        val result = countCharsAndBoolean(inputString)
        if (result.nonEmpty && result.size == 1) true
        else false
      case _ => false
    }
  }

  println(solution("czdaabb"))

}
