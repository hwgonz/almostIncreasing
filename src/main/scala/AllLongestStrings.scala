object AllLongestStrings extends App {

  def allLongestStrings(inputArray: Array[String]): Array[String] = {
    val higherSize = inputArray.map(item => item.length).max
    val result = inputArray.filter(item => item.length == higherSize )
    result
  }

  val testArr: Array[String] = Array("aba", "aa", "ad", "vcd", "aba")
  println(allLongestStrings(testArr).mkString("Array(", ", ", ")"))
}
