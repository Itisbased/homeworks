package hw

/**   I. Упражнения
  *
  * Задания в этом файле необходимо решать используя иммутабельные коллекции,
  * т.е. scala.collection._ и scala.collection.immutable._
  */

/** 1.
  *
  * Реализуйте метод removeMostFrequent. В списке чисел нужно найти число с
  * самым большим числом повторений, и вернуть новый список без этого числа Если
  * есть несколько разных чисел с одинаковой (максимальной) частотой, то удалить
  * их все
  */
def removeMostFrequent(numbers: Seq[Int]): Seq[Int] =
  val freqMap = numbers.groupBy(identity).view.mapValues(_.size).toMap
  val maxFreq = freqMap.values.maxOption.getOrElse(0)
  val toRemove = freqMap.collect { case (num, freq) if freq == maxFreq => num }.toSet
  numbers.filterNot(toRemove)

/** 2.
  *
  * Реализуйте метод smoothNumbers. Для каждого элемента списка, нужно заменить
  * его на среднее арифметическое этого элемента и двух соседних Если какого-то
  * из соседних элементов нет, то среднее необходимо считать не по 3, а по 2 или
  * 1 значению.
  */
def smoothNumbers(numbers: Seq[Int]): Seq[Double] =
  numbers.indices.map { i =>
    val neighbors = numbers.slice((i - 1) max 0, (i + 2) min numbers.length)
    neighbors.sum.toDouble / neighbors.size
  }

case class User(
  lastName: String,
  firstName: String,
  middleName: String,
  age: Int
)

/** 3.
  *
  * Реализуйте метод sortUsers. Есть список людей (фамилия, имя, отчество,
  * возраст) Нужно отсортировать его в следующем порядке: фамилия (лекс) ->
  * возраст (по убыванию) -> имя (лекс) -> отчество (лекс)
  */
def sortUsers(users: Seq[User]): Seq[User] =
  users.sortBy(u => (u.lastName, -u.age, u.firstName, u.middleName))

/** 4.
  *
  * Релизовать ленивый бесконечный список, состоящий из степеней двойки
  *
  * powersOfTwo = 2 #:: 4 #:: 8 #:: 16 #:: 32 ...
  */
val powersOfTwo: LazyList[BigInt] = LazyList.iterate(BigInt(2))(_ * 2)

/** 5.
  *
  * С помощью Решета Эратосфена реализовать ленивый бесконечный список,
  * состоящий из простых чисел sieveEratosthene = 2 #:: 3 #:: 5 #:: 7 #:: 11 #::
  * 13 #:: 17 ...
  *
  * В этой задаче не требуется оптимальный алгоритм, ожидается что хотя бы
  * вычисление первой 1000 простых чисел будет корректно работать.
  */
lazy val sieveEratosthene: LazyList[Int] =
  def sieve(stream: LazyList[Int]): LazyList[Int] =
    stream.head #:: sieve(stream.tail.filter(_ % stream.head != 0))
  sieve(LazyList.from(2))
