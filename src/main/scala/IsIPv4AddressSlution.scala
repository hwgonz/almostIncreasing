import scala.util.Try

/*
An IP address is a numerical label assigned to each device (e.g., computer, printer) participating in a computer network that uses the Internet Protocol for communication. There are two versions of the Internet protocol, and thus two versions of addresses. One of them is the IPv4 address.

Given a string, find out if it satisfies the IPv4 address naming rules.

Example

For inputString = "172.16.254.1", the output should be
solution(inputString) = true;

For inputString = "172.316.254.1", the output should be
solution(inputString) = false.

316 is not in range [0, 255].

For inputString = ".254.255.0", the output should be
solution(inputString) = false.

There is no first number.
 */

object IsIPv4AddressSlution extends App {

  def isInRange(number: Int): Boolean = number >= 0 && number < 256

  def onlyIntComponents(input: Array[String]): Boolean = {
    val filtered = input.map {
      comp =>
        val result = Try(comp.toInt).toOption
        result match {
          case Some(0) if comp != "0" => false
          case Some(value) if comp(0) == '0' && value > 0 => false
          case Some(_) => true
          case None => false
        }
    }.filter(_ == false)
    if (filtered.length > 0) false
    else true
  }

  def decomposeAddress(address: String): Boolean = {
    val preAddress = address.split('.')
    if (preAddress.length != 4)
      false
    else {
      val possibleValidAddress = preAddress.filter(comp => comp.nonEmpty)
      if (onlyIntComponents(possibleValidAddress)) {
        val result = possibleValidAddress.filter(comp => isInRange(comp.toInt))
        if (result.length == 4) true
        else
          false
      }
      else
        false
    }
  }

  def solution(inputString: String): Boolean = {
    decomposeAddress(inputString)
  }

  println(solution("172.16.254.1"))
  println(solution("172.316.254.1"))
  println(solution(".254.255.0"))
  println(solution("1.1.1.1a"))
  println(solution("0..1.0.0"))
  println(solution("64.233.161.00"))
  println(solution("01.233.161.131"))
  println(solution("0.254.255.0"))

}
