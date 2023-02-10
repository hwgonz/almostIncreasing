/*

Given a rectangular matrix of characters, add a border of asterisks(*) to it.

Example

For

picture = ["abc",
           "ded"]
the output should be

solution(picture) = ["*****",
                      "*abc*",
                      "*ded*",
                      "*****"]
 */

object AddBorderSolution extends App {
  def solution(picture: Array[String]): Array[String] = {
    val coverElement = "*" * picture.head.length + "**"
    val innerElements = for {
      element <- picture
    } yield "*" + element + "*"

    val initialList = coverElement :: innerElements.toList
    val fullList = initialList :+ coverElement
    fullList.toArray
  }

  val picture = Array("abc",
                 "ded")

  for {
    element <- solution(picture)
  } yield println(element)


}
