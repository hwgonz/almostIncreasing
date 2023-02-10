/**
 * Merge Overlapping Ranges.
 * Given a set of inclusive ranges [int, int],
 * compute a (potentially shorter) set of non-overlapping ranges.
 */

object RangeOverlapSolution extends App {

  case class Range(from:Int, to:Int)

  final def collapse(rs: List[Range], sep: List[Range] = Nil): List[Range] = rs match {
    case x :: y :: rest =>
      if (y.from > x.to) collapse(y :: rest, x :: sep)
      else collapse(Range(x.from, x.to max y.to) :: rest, sep)
    case _ =>
      (rs ::: sep).reverse
  }

  def merge(rs: List[Range]): List[Range] = collapse(rs.sortBy(_.from))

  println(merge(List(Range(0,4), Range(3,6), Range(7,9))))

}
