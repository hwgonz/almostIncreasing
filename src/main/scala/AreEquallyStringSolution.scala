/*
Call two arms equally strong if the heaviest weights they each are able to lift are equal.

Call two people equally strong if their strongest arms are equally strong (the strongest arm can be both the right and the left), and so are their weakest arms.

Given your and your friend's arms' lifting capabilities find out if you two are equally strong.

Example

For yourLeft = 10, yourRight = 15, friendsLeft = 15, and friendsRight = 10, the output should be
solution(yourLeft, yourRight, friendsLeft, friendsRight) = true;
For yourLeft = 15, yourRight = 10, friendsLeft = 15, and friendsRight = 10, the output should be
solution(yourLeft, yourRight, friendsLeft, friendsRight) = true;
For yourLeft = 15, yourRight = 10, friendsLeft = 15, and friendsRight = 9, the output should be
solution(yourLeft, yourRight, friendsLeft, friendsRight) = false.
 */
object AreEquallyStringSolution extends App {

  def solution(yourLeft: Int, yourRight: Int, friendsLeft: Int, friendsRight: Int): Boolean = {
    (yourLeft, yourRight, friendsLeft, friendsRight) match {
      case (yl, yr, fl, fr) if yl == fl && yr == fr => true
      case (yl, yr, fl, fr) if yl == fr && yr == fl => true
      case _ => false
    }
  }

  println(solution(10,15,15,10))
  println(solution(15,10,15,10))
  println(solution(15,10,15,9))

}
