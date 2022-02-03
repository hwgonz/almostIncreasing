import scala.annotation.tailrec

object LazyListUse extends App {

  sealed trait LazyList[+A] {

    def toList: List[A] = {
      @tailrec
      def go(ll: LazyList[A], acc: List[A]): List[A] =
        ll match {
          case Empty      => acc.reverse
          case Cons(h, t) => go(t(), h() :: acc)
        }
      go(this, Nil)
    }

    import LazyList._
    def take(n: Int): LazyList[A] =
      this match {
        case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
        case Cons(h, _) if n == 1 => cons(h(), empty)
        case _                    => empty
      }
  }
  case object Empty extends LazyList[Nothing]
  case class Cons[+A](h: () => A, t: () => LazyList[A]) extends LazyList[A]

  object LazyList {
    def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A]: LazyList[A] = Empty
    def apply[A](as: A*): LazyList[A] = {
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
    }
  }

  import LazyList._
  val fibs: LazyList[Int] = {
    def go(current: Int, next: Int): LazyList[Int] =
      cons(current, go(next, current + next))
    go(0, 1)
  }
  println(fibs.take(5).toList)
}
