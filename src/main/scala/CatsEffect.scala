import cats.CommutativeApply.ops.toAllCommutativeApplyOps
import cats.syntax.all._
import cats.effect._
import cats.effect.concurrent.MVar
import scala.concurrent.duration._

object CatsEffect extends IOApp {
  def runPrinter(mvar: MVar[IO, String]): Resource[IO, Unit] = {
    def rec: IO[Unit] = for {
      value <- mvar.take
      _ <- IO(println(value))
      _ <- rec
    } yield ()

    Resource.make(rec.start)(_.cancel.flatMap(_ => IO(println("printer closed")))).void
  }

  def runCounter(mvar: MVar[IO, String]): Resource[IO, Unit] = {
    def rec(counter: Int): IO[Unit] = for {
      _ <- mvar.put((counter).toString)
      _ <- IO.sleep(1.seconds)
      _ <- rec(counter + 1)
    } yield ()
    Resource.make(rec(0).start)(_.cancel.flatMap(_ => IO(println("counter closed")))).void
  }

  val gracefulShutdownProgram: Resource[IO, Unit] = for {
    mvar <- Resource.make(MVar.empty[IO, String])(_ => IO(println("release")))
    _ <- runPrinter(mvar)
    _ <- runCounter(mvar)
  } yield ()

  override def run(args: List[String]): IO[ExitCode] = gracefulShutdownProgram.use(_ => IO.never)
}