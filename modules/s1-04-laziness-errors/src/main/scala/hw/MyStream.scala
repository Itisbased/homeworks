package hw

/**
 * MyStream - ленивый список, аналогичный LazyList в Scala
 */
sealed trait MyStream[+A]:
  import MyStream.*

  def take(n: Int): List[A] = takeInner(this, n, Nil).reverse

  /* Реализуйте метод фильтрации ленивого списка. Результатом тоже должен быть ленивый список */
  def filter(predicate: A => Boolean): MyStream[A] = this match {
    case MyStream.Empty => MyStream.Empty
    case NonEmpty(h, t) =>
      if predicate(h()) then MyStream(h(), t().filter(predicate))
      else t().filter(predicate)
  }

  /* Реализуйте метод, который лениво отбросит все элементы, пока выполняется условие на элемент */
  def dropWhile(predicate: A => Boolean): MyStream[A] = this match {
    case MyStream.Empty => MyStream.Empty
    case NonEmpty(h, t) =>
      if predicate(h()) then t().dropWhile(predicate)
      else this
  }

  /* Реализуйте метод flatMap, аналогичный такому у списка. Метод должен работать лениво */
  def flatMap[B](f: A => MyStream[B]): MyStream[B] = this match {
    case MyStream.Empty => MyStream.Empty
    case NonEmpty(h, t) =>
      def append(s1: MyStream[B], s2: => MyStream[B]): MyStream[B] = s1 match {
        case MyStream.Empty   => s2
        case NonEmpty(h1, t1) => MyStream(h1(), append(t1(), s2))
      }
      append(f(h()), t().flatMap(f))
  }

object MyStream {
  case object Empty extends MyStream[Nothing]
  case class NonEmpty[A] private[MyStream] (h: () => A, t: () => MyStream[A]) extends MyStream[A]

  def apply[A](h: => A, t: => MyStream[A]): MyStream[A] =
    NonEmpty(() => h, () => t)

  @annotation.tailrec
  private def takeInner[A](stream: MyStream[A], n: Int, acc: List[A]): List[A] =
    stream match {
      case NonEmpty(h, t) if n > 0 => takeInner(t(), n - 1, h() :: acc)
      case _                       => acc
    }
}
