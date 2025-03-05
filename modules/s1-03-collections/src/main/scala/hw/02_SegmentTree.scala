package hw

trait Monoid[A]:
  def empty: A
  def combine(l: A, r: A): A

object Monoid:
  val intSum = new Monoid[Int]:
    def empty: Int = 0
    def combine(l: Int, r: Int): Int = l + r

  val stringConcat = new Monoid[String]:
    def empty: String = ""
    def combine(l: String, r: String): String = l + r

/**
  * II. Дерево отрезков
  *
  * Необходимо реализовать структуру данных -- дерево отрезков (подробнее можно прочитать на хабре https://habr.com/ru/articles/808511/)
  * Дерево отрезков позволяет эффективно (логарифмическая сложность) считать результат ассоциативной функции на отрезке и обновлять значения в массиве.
  *
  */
sealed trait SegmentTree[A]:
  /**
    * Предпосчитанный результат выполнения операции
    */
  def value: A

  /**
    * начало интервала исходной последователености
    * корорый "связан" с этим деревом
    */
  def startIdx: Int

  /**
    * конец интервала исходной последователености
    * корорый "связан" с этим деревом
    */
  def endIdx: Int

  /**
    * aссоциативная операция и нейтральный элемент
    */
  def monoid: Monoid[A]

  /**
  * II. 2.
  *
  * Реализовать метод update который обновляет эелемент с индеком idx на значение a
  * Должен иметь логарифмическую сложность от размера исходной последовательности
  *
  * Не обязательно использовать хвостовую рекурсию (на лекции по ленивости будем разбирать как такие вызовы делать стэкобезопасными).
  *
  * Пример:
  *  Если дерево построено на последовательности (5, 2, 4, 1, 6, 7)
  *  То:
  *    update(3, 0) должен будет вернуть дерево:
  *
  *                     24
  *                 /       \
  *               11         13
  *             /   \      /    \
  *            7     4   13     0
  *          /  \   / \   / \   / \
  *         5    2 4   0 6   7 0   0
  */
  def update(idx: Int, a: A): SegmentTree[A] = this match
    case Leaf(i, _, monoid) if i == idx => Leaf(i, a, monoid)
    case Node(_, start, end, left, right, monoid) =>
      if idx <= left.endIdx then
        val newLeft = left.update(idx, a)
        Node(monoid.combine(newLeft.value, right.value), start, end, newLeft, right, monoid)
      else
        val newRight = right.update(idx, a)
        Node(monoid.combine(left.value, newRight.value), start, end, left, newRight, monoid)
    case _ => this

  /**
  * II. 3.
  *
  * Реализовать метод calc который считает результат функции (которая была указано неявно при создании дерева)
  * на отрезке [from, to] (оба конца включительно) используя частичные результаты посчитанные в узлах дерева.
  *
  * Должен иметь логарифмическую сложность от размера интервала.
  *
  * Не обязательно использовать хвостовую рекурсию (на лекции по ленивости будем разбирать как такие вызовы делать стэкобезопасными).
  *
  * Пример:
  *  SegmentTree(5, 2, 4, 1, 6, 7)(Monoid.intSum).calc(1, 5) = 20
  *  SegmentTree(5, 2, 4, 1, 6, 7)(Monoid.intSum).calc(1, 4) = 13
  */
  def calc(from: Int, to: Int): A = this match
    case Leaf(idx, value, monoid) if from <= idx && idx <= to => value
    case Leaf(_, _, monoid) => monoid.empty
    case Node(value, start, end, left, right, monoid) =>
      if to < start || from > end then monoid.empty
      else if from <= start && end <= to then value
      else monoid.combine(left.calc(from, to), right.calc(from, to))

case class Leaf[A](idx: Int, value: A, monoid: Monoid[A]) extends SegmentTree[A]:
  val startIdx = idx
  val endIdx = idx

case class Node[A](
  value: A,
  startIdx: Int,
  endIdx: Int,
  left: SegmentTree[A],
  right: SegmentTree[A],
  monoid: Monoid[A]
) extends SegmentTree[A]

object SegmentTree:
  /**
   * II. 1.
   *
   * Реализовать метод apply, который строит дерево из последовательности элементов
   *
   * Пример:
   *   Для последовательности SegmentTree(5, 2, 4, 1, 6, 7)
   *   (монойд по для Int по умолчанию по сложению)
   *   Должно быть построено дерево:
   *
   *                     25
   *                 /       \
   *               12         13
   *             /   \      /    \
   *            7     5    13     0
   *          /  \   / \   / \   / \
   *         5    2 4   1 6   7 0   0
   */
  def apply[A](values: A*)(monoid: Monoid[A]): SegmentTree[A] =
    def build(start: Int, end: Int): SegmentTree[A] =
      if start == end then
        Leaf(start, values(start), monoid)
      else
        val mid = (start + end) / 2
        val left = build(start, mid)
        val right = build(mid + 1, end)
        Node(monoid.combine(left.value, right.value), start, end, left, right, monoid)

    build(0, values.length - 1)
  /**
    * II. 4. Вопрос: как будет выглядеть код, если вам потребуется считать результаты нескольких операций
    *    на одной одной и той же последовательности элементов?
    *
    *    Потребуется ли делать дополнительную структуру данных? Или расширять текущую?
    */
