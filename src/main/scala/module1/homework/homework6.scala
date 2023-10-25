package module1.homework

import scala.annotation.tailrec
import scala.language.higherKinds


object homework6 {
Option

  sealed trait OptionError[+A]
  case object ENone extends OptionError[Nothing]
  case class ESome[+A](a: A) extends OptionError[A]
  case object EError extends OptionError[Nothing]


  trait Show[T] {
    def show(v: T): String
  }

  object Show {
    def apply[T](implicit s: Show[T]): Show[T] = s
    def fromFunction[T](f: T => String): Show[T] = (v: T) => f(v)
    def fromToString[T]: Show[T] = (v: T) => v.toString

    implicit val strShow: Show[String] = Show.fromFunction[String](v => s"[String] ${v}")
    implicit val intShow: Show[Int] = Show.fromFunction[Int](v => s"[Int] ${v}")
    implicit val booleanShow: Show[Boolean] = Show.fromFunction[Boolean](v => s"[Boolean] ${v}")

    implicit def optionErrorShow[A](implicit valShow: Show[A]): Show[OptionError[A]] = Show.fromFunction[OptionError[A]] {
      case ENone => "None"
      case ESome(a) => valShow.show(a)
      case EError => "Error!!!"
    }
    implicit def listShow[A](implicit valShow: Show[A]): Show[List[A]] = Show.fromFunction[List[A]] {
      case Nil => "[EmptyList]"
      case ::(head, tl) =>
        tl.foldLeft(s"[List] [${valShow.show(head)}")((s: String, v: A) => s"${s} :: ${valShow.show(v)}") + "]"
    }

    implicit def setShow[A](implicit valShow: Show[A]): Show[Set[A]] = Show.fromFunction[Set[A]](s =>
        s.foldLeft(None: Option[String])((ac: Option[String], v: A) => ac match {
          case Some(x) => Some(s"${x} :: ${valShow.show(v)}")
          case None => Some(valShow.show(v))
        }) match {
          case Some(x) => s"[Set] [${x}]"
          case None => "[EmptySet]"
        })

    implicit val doubleShow: Show[Double] = Show.fromToString[Double]
    implicit def optionShow[A](implicit valShow: Show[A]): Show[Option[A]] = Show.fromFunction[Option[A]] {
      case Some(x) => valShow.show(x)
      case None => "None"
    }

    implicit class ShowSyntax[T](v: T){
      def show(implicit vs: Show[T]): String = vs.show(v)
    }

  }

  // Monad

  trait Monad[F[_]] {
    def flatMap[A,B](fa: F[A])(f: (A) => F[B]): F[B]
    def map[A, B](fa: F[A])(f: (A) ⇒ B): F[B]
    def pure[A](x:A): F[A]
    def flatten[A](fa: F[F[A]]): F[A] = flatMap(fa)(x => x)
  }

  object Monad {
    def apply[T[_]](implicit s: Monad[T]): Monad[T] = s
    implicit def optionMonad: Monad[Option] = new Monad[Option] {
      def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
        case Some(x) => f(x)
        case None => None
      }

      override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

      override def pure[A](x: A): Option[A] = Some(x)
    }

    implicit def listMonad: Monad[List] = new Monad[List] {
      override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

      override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

      override def pure[A](x: A): List[A] = List(x)
    }

    implicit def optionErrorMonad: Monad[OptionError] = new Monad[OptionError] {
      override def flatMap[A, B](fa: OptionError[A])(f: A => OptionError[B]): OptionError[B] = fa match {
        case ENone => ENone
        case ESome(a) => f(a)
        case EError => EError
      }

      override def map[A, B](fa: OptionError[A])(f: A => B): OptionError[B] = fa match {
        case ENone => ENone
        case ESome(a) => ESome(f(a))
        case EError => EError
      }

      override def pure[A](x: A): OptionError[A] = ESome(x)
    }

    // не получается переопределить ситаксис .flatten для встроенных типов, все равно вызывается
    // flatten из базового класса, работает только для своего класса, в котором нет метода flatten.
    implicit class MonadFlattenSyntax[T[_],A](a: T[T[A]]){
      def flattenMy(implicit m: Monad[T]): T[A] = m.flatten(a)
      def flatten(implicit m: Monad[T]): T[A] = m.flatten(a)
    }


  }

}
