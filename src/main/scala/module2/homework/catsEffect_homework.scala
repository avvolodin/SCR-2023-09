package module2.homework


import cats.Functor.ops.toAllFunctorOps
import cats.effect.{ExitCode, IO, IOApp}

import scala.language.higherKinds
import scala.util.Try


object catsEffectHomework {

  /**
   * Тайп класс для генерации псевдо случайных чисел
   *
   * @tparam F
   */
  trait Random[F[_]] {
    /**   *
     *
     * @param min значение от (включительно)
     * @param max значение до (исключается)
     * @return псевдо случайное число в заданном диапазоне
     */
    def nextIntBetween(min: Int, max: Int): F[Int]
  }


  object Random {
    /**
     * 1. реализовать сумонер метод для класса Random, в последствии он должен позволить
     * использовать Random например вот так для IO:
     * Random[IO].nextIntBetween(1, 10)
     *
     * @return Random[F]
     */
    def apply[T[_]](implicit v: Random[T]): Random[T] = v

    /**
     * 2. Реализовать инстанс тайп класса для IO
     */
    implicit val ioRandom = new Random[IO] {
      val r = scala.util.Random

      /** *
       *
       * @param min значение от (включительно)
       * @param max значение до (исключается)
       * @return псевдо случайное число в заданном диапазоне
       */
      override def nextIntBetween(min: Int, max: Int): IO[Int] = IO.fromEither {
        if (max <= min) Left(new Throwable("max should be grater then min")) else Right(r.nextInt(max - min) + min)
      }
    }
  }

  /**
   * Тайп класс для совершения операций с консолью
   *
   * @tparam F
   */
  trait Console[F[_]] {
    def printLine(str: String): F[Unit]

    def readLine(): F[String]
  }

  object Console {
    /**
     * 3. реализовать сумонер метод для класса Console, в последствии он должен позволить
     * использовать Console например вот так для IO:
     * Console[IO].printLine("Hello")
     *
     * @return Console[F]
     */
    def apply[T[_]](implicit v: Console[T]): Console[T] = v

    /**
     * 4. Реализовать инстанс тайп класса для IO
     */
    implicit val ioConsole = new Console[IO] {
      override def printLine(str: String): IO[Unit] = IO.fromTry(Try {
        println(str)
      })

      override def readLine(): IO[String] = IO.fromTry(Try {
        scala.io.StdIn.readLine
      })
    }
  }

  /**
   * 5.
   * Используя Random и Console для IO, напишите консольную программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Программа должна выполняться до тех пор, пока пользователь не угадает.
   * Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  def guess(secret: Int): IO[Unit] = for {
    line <- Console[IO].readLine()
    number <- IO {
      Try {
        Integer.parseInt(line)
      }.getOrElse(0)
    }
    _ <- if (secret != number) {
      for {
        _ <- Console[IO].printLine("Wrong!")
        _ <- guess(secret)
      } yield {
        ()
      }
    } else IO {
      ()
    }
  } yield ()

  val guessProgram = for {
    _ <- Console[IO].printLine("Guess the number (from 1 to 3)")
    secret <- Random[IO].nextIntBetween(1, 4)
    _ <- guess(secret)
    _ <- Console[IO].printLine("Right!")
  } yield ()


  /**
   * 6. реализовать функцию doWhile (общего назначения) для IO, которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * Подумайте над сигнатурой, еам нужно принимать эффект и условие относительно его значения, для того чтобы повторять либо заканчивать выполнение.
   */

  def doWhile[T](pr: T => Boolean)(ef: IO[T]): IO[Unit] =
    for {
      r <- ef
      _ <- if (pr(r)) IO {
        ()
      } else ef
    } yield ()
}

/**
 * 7. Превратите данный объект в исполняемую cats effect программу, которая будет запускать
 * guessProgram
 */
object HomeworkApp extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = catsEffectHomework.guessProgram.as(ExitCode.Success)
}
