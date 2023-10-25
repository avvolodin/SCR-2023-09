import module1.homework.homework6.{EError, ESome, Monad, OptionError}
import module1.homework.homework6.Monad.MonadFlattenSyntax
import module1.homework.homework6.Show.ShowSyntax
import module1.implicits.{implicit_conversions, implicit_scopes}
import module1.threads.{Thread1, ToyFuture, getRatesLocation1, getRatesLocation2, getRatesLocation3, getRatesLocation4, printRunningTime}
import module1.{executor, future, hof, lazyOps, list, try_, type_system}

import scala.collection.immutable
import scala.concurrent.Future


object Main {

  def main(args: Array[String]): Unit = {
//    println("Hello, World!" +
//      s" thread - ${Thread.currentThread().getName}" )

//    val t1 = new Thread{
//      override def run(): Unit ={
//        Thread.sleep(1000)
//        println(s"Hello ${Thread.currentThread().getName}" )
//      }
//    }
//    val t2 = new Thread{
//      override def run(): Unit ={
//        Thread.sleep(2000)
//        println(s"Hello ${Thread.currentThread().getName}" )
//      }
//    }
//    t2.start()
//    t1.start()

//    def rates = {
//       val t1 = ToyFuture(10)(executor.pool1)
//       val t2 = ToyFuture(20)(executor.pool1)
//
//       t1.onComplete{ i1 =>
//         t2.onComplete{i2 =>
//           println(i1 + i2)
//         }
//       }
//
//       val r: ToyFuture[Unit] = for{
//         i1 <- t1
//         i2 <- t2
//       } yield println(i1 + i2)
//    }
//
//    printRunningTime(rates)

//    try_.readFromFile2().foreach(println(_))

    import scala.concurrent.ExecutionContext.Implicits.global

//    val f1 = future.getRatesLocation1
//    val f2 = future.getRatesLocation2
//
//    def sum(v1: Int, v2: Int): Future[Int] = ???
//
//    def zip[T, B](f1: Future[T], f2: Future[B]] = for{
//      r1 <- f1
//      r2 <- f2
//      r3 <- sum(r1, r2)
//    }  yield r3
//
//    future.getRatesLocation1.flatMap(r1 =>
//      future.getRatesLocation2.map(r2 => r1 + r2)
//    )


//
//    future.printRunningTime(
//      future.f7
//    )
//
//    Thread.sleep(4000)

//    implicit_scopes

    println(List.empty[Int].show)

    val o: Option[Option[Int]] = Some(Some(10))

    val s1: Option[Int] = o.flattenMy


    val opInt: Option[Int] = Monad[Option].pure(1)
    val opopInt: Option[Option[Int]] = Monad[Option].pure(opInt)

    println(opopInt.flattenMy.show)

    val lInt: List[Int] = Monad[List].pure(1) ::: Monad[List].pure(2)
    val llInt: List[List[Int]] = Monad[List].pure(lInt) ::: Monad[List].pure(lInt)

    opopInt.flattenMy

    println(llInt.flattenMy(Monad[List]))

    val oe: OptionError[Int] = EError
    val oeoe: OptionError[OptionError[Int]] = ESome(oe)

    println(oeoe.flatten.show)






  }
}