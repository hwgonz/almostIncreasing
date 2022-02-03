object SortByHeight extends App {

  def sortByHeight(a: Array[Int]): Array[Int] = {
    val aWithIndexes = a.zipWithIndex
    val onlyTrees = aWithIndexes.filter(item => item._1 == -1)
    val (personsSorted, positions) = aWithIndexes.filter(item => item._1 != -1).sorted.unzip
    val sortedPositions = positions.sorted
    val onlyPersonsSorted = personsSorted zip sortedPositions
    val fullArray = onlyTrees ++ onlyPersonsSorted
    val result = fullArray.sortBy(_._2)
      .map(_._1)
    result
  }

  val anArray: Array[Int] = Array(-1, 150, 190, 170, -1, -1, 160, 180)
  println(sortByHeight(anArray).mkString("Array(", ", ", ")"))
}
