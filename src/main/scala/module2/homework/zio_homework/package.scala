package module2.homework

import zio.{Has, Layer, Task, UIO, ULayer, URIO, ZIO, ZLayer}
import zio.clock.{Clock, currentTime, nanoTime, sleep}
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.io.StdIn
import scala.language.postfixOps
import scala.util.Try

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  def r = Random.Service.live.nextIntBetween(1,4)


  lazy val guessProgram: ZIO[Console with Random, Throwable, Boolean] = for {
    randomInt <- nextIntBetween(1,4)
    _ <- putStrLn("Guess the number (from 1 to 3):")
    strValue <- getStrLn
    intValue <- ZIO.effect(Try(Integer.parseInt(strValue)).getOrElse(0))
    r <- ZIO.effect(if(intValue == randomInt) "Hit!" else "Miss!")
    _ <- putStrLn(r)
  } yield(intValue == randomInt)

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile[R, E]: ZIO[R, E, Boolean] => ZIO[R, E, Boolean] = (z: ZIO[R, E, Boolean])=>
    ZIO.iterate[R,E,Boolean](false)(v => !v)(v => z)

  /**
   * 3. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 3.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: ZIO[Random with Clock, Nothing, Int] = for {
    _ <- ZIO.sleep(1.second)
    value <- nextIntBetween(0, 11)
  } yield value

  /**
   * 3.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects = List.range(0, 10).map(r => eff)
  
  /**
   * 3.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app = ZIO.reduceAll(ZIO.succeed(0), effects)((c, v) => c+v)


  /**
   * 3.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp = ZIO.reduceAllPar(ZIO.succeed(0), effects)((c,v)=>c+v)


  /**
   * 4. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */

  type RunningTimeEnv = Has[RunningTime.Service]

  case class User(name: String, email: String)
  object RunningTime {
    // service definition
    trait Service {
      def start(): ZIO[Any, Throwable, Unit]
      def stop(): ZIO[Any, IOException, Unit]
    }

    // layer; includes service implementation
    val live: ZLayer[Console with Clock, Nothing, RunningTimeEnv] =
      ZLayer.fromServices[Console.Service, Clock.Service, RunningTime.Service]{
        (console, clock) => new Service{
      var time: Long = 0
      override def start(): ZIO[Any, Throwable, Unit] = for{
        s <- clock.currentTime(TimeUnit.MILLISECONDS)
        _ <- Task {
          time = s
        }
      } yield ()


      override def stop(): ZIO[Any, IOException, Unit] = {
        for {
          ct <- clock.currentTime(TimeUnit.MILLISECONDS)
          _ <- console.putStrLn(s"Time spent: ${ct - time}")
        } yield ()

      }
    }}

    // accessors
    def start(): ZIO[RunningTimeEnv, Throwable, Unit] = ZIO.accessM(_.get.start())
    def stop(): ZIO[RunningTimeEnv, Throwable, Unit] = ZIO.accessM(_.get.stop())
  }


   /**
     * 5.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg = for {
    _ <- RunningTime.start()
    v <- appSpeedUp
    _ <- RunningTime.stop()
  } yield v

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp = for {
    s <- zio_homework.appWithTimeLogg
    _ <- ZIO.effect(println(s))
  } yield ()

}
