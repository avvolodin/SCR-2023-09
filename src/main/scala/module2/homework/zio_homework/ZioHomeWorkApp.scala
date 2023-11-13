package module2.homework.zio_homework

import module2.homework.zio_homework.RunningTime.live
import zio.clock.Clock
import zio.console.Console
import zio.random.Random
import zio.{ExitCode, URIO, ZIO}

object ZioHomeWorkApp extends zio.App {
  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, ExitCode] = {
    val ur = (Random.live ++ Clock.live ++ Console.live ++ RunningTime.live)
    runApp.provideLayer(ur)
      .catchAll(t => ZIO.succeed(ExitCode.failure))
      .map( v =>
        ExitCode.success
      )
  }
}
