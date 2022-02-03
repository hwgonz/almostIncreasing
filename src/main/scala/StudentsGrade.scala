object StudentsGrade extends App {
  val rightAnswers: Array[(Char, Int)] = Array('A', 'A', 'C', 'D').zipWithIndex
  val studentsAnswers: Map[String, Array[(Char, Int)]] = Map(
    "Alice" -> Array('B', 'A', 'C', 'D').zipWithIndex,
    "Bob" -> Array('A', 'A', 'C', 'D').zipWithIndex
  )

  def gradeStudent(answers: Array[(Char, Int)]): Double = {
    val result = answers.foldLeft(0) { (acc, current) =>
      if (current._1 == rightAnswers(current._2)._1) acc + 1 else acc
    }

    println(result / answers.length)
    result / answers.length
  }

  val results = studentsAnswers.map(student => gradeStudent(student._2))
  println(s"The class average is: ${results.sum / results.size}")

}
