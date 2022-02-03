import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

object FutureSequence extends App {

  def producer = {

    val first = Future {
      println("startFirst")
      Thread.sleep(3000)
      println("stopFirst")
    }

    val second = Future {
      println("startSecond")
      Thread.sleep(1000)
      println("stopSecond")
    }

    val list = Seq(
      first,
      second
    )

    for {
      _ <- first
      _ <- second
    } yield ()

    //Future.sequence(list)
  }

  Await.result(producer, Duration.Inf)

}
