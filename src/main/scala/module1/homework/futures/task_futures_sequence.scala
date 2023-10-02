package module1.homework.futures

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] =
  {
    val p = Promise[(List[A], List[Throwable])]()

    def rec(futures: List[Future[A]], result: (List[A], List[Throwable])): Unit = futures match {
      case Nil => p.success((result._1.reverse, result._2.reverse))
      case ::(head, tl) => head.onComplete{
        case Success(value) => rec(tl, (value :: result._1, result._2))
        case Failure(throwable) => rec(tl, (result._1, throwable :: result._2))
      }
    }
    rec(futures, (List.empty[A], List.empty[Throwable]))
    p.future
  }

}
