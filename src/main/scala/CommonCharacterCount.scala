object CommonCharacterCount extends App {

  def commonCharacterCount(s1: String, s2: String): Int = {
    val s1AsArrayOfChar = s1.toCharArray.sorted
    val s2AsArrayOfChar = s2.toCharArray.sorted

    val result = if (s1AsArrayOfChar.length >= s2AsArrayOfChar.length) {
      val difference = s1AsArrayOfChar diff s2AsArrayOfChar
      println(difference.mkString("Array(", ", ", ")"))
      s1AsArrayOfChar.length - difference.length
    } else {
      val difference = s2AsArrayOfChar diff s1AsArrayOfChar
      s2AsArrayOfChar.length - difference.length
    }
    result
  }

  val s1 = "aabcc"
  val s2 = "adcaa"
  println(commonCharacterCount(s1, s2))
}
