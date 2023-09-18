package module1

import java.util.UUID
import scala.annotation.tailrec
import java.time.Instant
import scala.language.postfixOps



/**
 * referential transparency
 */



 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n){
      _n *= i
      i += 1
    }
    _n
  }

  def factRec(n: Int): Int =
    if( n <= 0) 1 else n * factRec(n - 1)

  def factRecTail(n: Int): Int = {

    @tailrec
    def loop(n: Int, accum: Int): Int =
      if( n <= 1) accum
      else loop(n - 1, n * accum)

    loop(n, 1)
  }



  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */


}

object hof{


  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
    val start = System.currentTimeMillis()
    val result: B = f(a)
    val end = System.currentTimeMillis()
    println(end - start)
    result
  }

  def doomy(str: String) = {
    Thread.sleep(1000)
    println(str)
  }



  // изменение поведения ф-ции

  val arr = Array(1, 2, 3, 4, 5)

  def isOdd(i: Int): Boolean = i % 2 > 0

  def not[A](f: A => Boolean): A => Boolean = a => !f(a)

  def isEven: Int => Boolean = not(isOdd)


  // (A, B, C) => D   curring A => B => C => D
  // изменение самой функции

  def curried[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)

  def partial[A, B, C](a: A, f: (A, B) => C): B => C = curried(f)(a)

  def sum(x: Int, y: Int): Int  = x + y

  val s0: Int => Int => Int = curried(sum)

  val s1: Function1[Int, Int] = s0(2)

  val s2: Int = s1(3) // 5

  val p0: Int => Int = partial(2, sum)

  p0(3) // 5


















  trait Consumer{
       def subscribe(topic: String): Stream[Record]
   }

   case class Record(value: String)

   case class Request()
   
   object Request {
       def parse(str: String): Request = ???
   }

  /**
   *
   * (Опционально) Реализовать ф-цию, которая будет читать записи Request из топика,
   * и сохранять их в базу
   */
   def createRequestSubscription() = ???



}






/**
 *  Реализуем тип Option
 */


 object opt {

  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутсвие результата
   */

  sealed trait Option[+T]{

    def isEmpty: Boolean =  this match {
      case Some(v) => false
      case None => true
    }

    def get: T = this match {
      case Some(v) => v
      case None => throw new Exception("None get")
    }

    def map[B](f: T => B): Option[B] = flatMap(v => Some(f(v)))

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Some(v) => f(v)
      case None => None
    }

    /**
     *
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */
    def printIfAny(): Unit = this match {
      case Some(v) => println(v)
      case None =>
    }

    /**
     *
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */

    def zip[A](a: Option[A]) : Option[(T, A)] = (this, a) match {
      case (Some(x), Some(y)) => Some((x,y))
      case _ => None
    }


    /**
     *
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */
    def filter(f: T=>Boolean): Option[T] = this match {
      case Some(v) if(f(v)) => Some(v)
      case _ => None
    }
  }
  case class Some[T](v: T) extends Option[T]
  case object None extends Option[Nothing]

  object Option {


  }

 }

 object list {
   /**
    *
    * Реализовать односвязанный иммутабельный список List
    * Список имеет два случая:
    * Nil - пустой список
    * Cons - непустой, содердит первый элемент (голову) и хвост (оставшийся список)
    */

    sealed trait List[+T] {
     /**
      * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
      *
      */
     def ::[A >: T](v: A): List[A] = new::(v, this)

     /**
      * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
      *
      */

     def mkString(divider: String = ","): String = {
       val builder = new StringBuilder()

       var ptr = this
       while (ptr != Nil) {
         ptr match {
           case ::(head, tail) => {
             if (builder.nonEmpty) builder.append(divider)
             builder.append(head)
             ptr = tail
           }
           case _ =>
         }

       }
       builder.toString()
     }

     def mkStringRec(divider: String = ","): Unit = {
       val builder = new StringBuilder()

       @tailrec
       def loop(res: List[T], builder: StringBuilder): Unit = {
         res match {
           case ::(head, tail) =>
             if (builder.nonEmpty) builder.append(divider)
             builder.append(head)
             loop(tail, builder)
           case Nil =>
         }
       }

       loop(this.reverse(), builder)
       builder.toString

     }

     /**
      * Конструктор, позволяющий создать список из N - го числа аргументов
      * Для этого можно воспользоваться *
      *
      * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
      * def printArgs(args: Int*) = args.foreach(println(_))
      */

     /**
      *
      * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
      */

     def reverse(): List[T] = {
       var newList: List[T] = Nil

       @tailrec
       def rec(list: List[T]): Unit = list match {
         case ::(head, tail) =>
           newList = head :: newList
           rec(tail)
         case Nil =>
       }

       rec(this)
       newList
     }

     def reverseIterative(): List[T] = {
       var ptr = this
       var newList: List[T] = Nil
       while (ptr != Nil) {
         ptr match {
           case ::(head, tail) =>
             newList = new::(head, newList)
             ptr = tail
           case Nil =>
         }
       }
       newList
     }

     /**
      *
      * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
      */

     def map[A](f: T => A): List[A] = {
       var ptr = this
       var newList: List[A] = Nil
       while (ptr != Nil) {
         ptr match {
           case ::(head, tail) =>
             newList = new::(f(head), newList)
             ptr = tail
           case Nil =>
         }
       }
       newList
     }

     def mapRec[A](f: T => A): List[A] = {
       var newList: List[A] = Nil

       @tailrec
       def rec(l: List[T]): Unit = l match {
         case ::(head, tail) =>
           newList = f(head) :: newList
           rec(tail)
         case Nil =>
       }

       rec(this.reverse())
       newList
     }

     def flatMap[A](f: T => List[A]): List[A] = {
       var ptr = this
       var newList: List[A] = Nil
       while (ptr != Nil) {
         ptr match {
           case ::(head, tail) =>
             var innerPtr = f(head)
             while (innerPtr != Nil) {
               innerPtr match {
                 case ::(head, tail) =>
                   newList = head :: newList
                   innerPtr = tail
                 case Nil =>
               }
             }
             ptr = tail
           case Nil =>
         }
       }
       newList.reverse()
     }

     def flatMapRec[A](f: T => List[A]): List[A] = {
       var newList: List[A] = Nil

       @tailrec
       def innerRec(l: List[A]): Unit = l match {
         case ::(head, tail) =>
           newList = head :: newList
           innerRec(tail)
         case Nil =>
       }

       @tailrec
       def rec(l: List[T]): Unit = l match {
         case ::(head, tail) =>
           innerRec(f(head))
           rec(tail)
         case Nil =>
       }

       rec(this)

       newList.reverse()

     }

     /**
      *
      * Реализовать метод filter для списка который будет фильтровать список по некому условию
      */

     def filter(p: T => Boolean): List[T] = {
       var ptr = this
       var newList: List[T] = Nil
       while (ptr != Nil) {
         ptr match {
           case ::(head, tail) =>
             if (p(head))
               newList = new::(head, newList)
             ptr = tail
           case Nil =>
         }
       }
       newList
     }

     def filterRec(p: T => Boolean): List[T] = {
       var newList: List[T] = Nil

       @tailrec
       def rec(list: List[T]): Unit = list match {
         case ::(head, tail) =>
           if(p(head))
             newList = head :: newList
           rec(tail)
         case Nil =>
       }
       rec(this)

       newList.reverse()
     }
   }
    case class ::[A](head: A, tail: List[A]) extends List[A]
    case object Nil extends List[Nothing]


    object List{
      // конструктор (recursive, tail optimization isn't possible)
      def applyRec[A](v: A*): List[A] = if(v.isEmpty) Nil
      else ::(v.head, applyRec(v.tail:_*))

      // recursive, tail optimised
      def applyRecTail[A](v: A*): List[A] = {
        var l : List[A] = Nil
        @tailrec
        def builder(a: Seq[A]) : Unit = if(a.nonEmpty) {
          l = a.head :: l
          builder(a.tail)
        }

        builder(v.reverse)
        l
      }

      // iterative
      def apply[A](v: A*): List[A] = {
        var l : List[A] = Nil
        for(value <- v.reverse)
          l = value :: l
        l
      }
    }

    // Пример создания экземпляра с помощью конструктора apply

    List(1, 2, 3)



    /**
      *
      * Написать функцию incList котрая будет принимать список Int и возвращать список,
      * где каждый элемент будет увеличен на 1
      */

   def incList(l: List[Int]): List[Int] = l.map(v=>v + 1)

    /**
      *
      * Написать функцию shoutString котрая будет принимать список String и возвращать список,
      * где к каждому элементу будет добавлен префикс в виде '!'
      */
   def shoutString(l: List[String]): List[String] = l.map(v => v + "!")

   def testApply(f: (Double, Double, Double, Double) => List[Double]): Long = {
     var a1 = Math.random()
     var a2 = Math.random()
     var a3 = Math.random()
     var a4 = Math.random()
     var t = 0.0
     val start = System.currentTimeMillis()
     for (i <- 1 to 10000000) {
       val l = f(a1,a2,a3,a4)
       l match {
         case ::(head, tail) => t = head
         case Nil => t = 0
       }
     }
     var end = System.currentTimeMillis()
     end - start
   }

   def stat(f: () => Long, rep: Int = 20): (Double, Double) = {
     val m = (1 to rep).map(v => f())
     val mean = m.sum.toDouble / m.length.toDouble
     val sd = Math.sqrt(m.map(v=>Math.pow(v-mean, 2)).sum / m.length)
     (mean, sd)
   }


 }