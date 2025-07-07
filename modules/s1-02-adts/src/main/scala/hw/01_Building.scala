package hw

/**
  * I. Здание и ADT
  *
  *   1. Отразить в ADT следующую предметную область:
  *      - Здание. У здания есть строковый адрес и этажи (ссылка на 1-й этаж)
  *      - Этаж бывает жилым, чердаком или коммерческим
  *      - У каждого жилого этажа есть 2 постояльца и лестница на следующий этаж
  *        (просто ссылка на этаж)
  *      - У каждого коммерческого этажа есть несколько заведений (минимум 1) и
  *        лестница на следующий этаж (да, в этом доме можно открыть свою
  *        кальянную на 5 этаже, даже если всего этажей 10 :kekw:)
  *      - У заведения есть название
  *      - Чердак может быть обычным, либо тоже коммерческим, но только с 1
  *        заведением.
  *      - У постояльца есть возраст и пол (м/ж).
  *
  *   2. Реализовать функцию fold аккумулирующую результат во время обхода здания.
  *      На каждом этаже аккумулируемое значение пересчитывается с помощью функции f,
  *      параметрами которой служат текущее значение аккумулятора и этаж. Первый
  *      параметр - это текущий аккумулятор. Второй \- текущий этаж. Здание
  *      необходимо обходить снизу вверх. Аккумулятор изначально равен accumulator
  *
  *   3. Реализовать остальные функции с помощью fold
  *
  *   4. Напишите тесты в Building.test.scala. На функцию fold отдельные тесты
  *      писать не обязательно
  */

import scala.annotation.tailrec

enum Gender:
  case Male, Female

case class Resident(age: Int, gender: Gender)

case class Building(address: String, firstFloor: Building.Floor)

object Building:
  case class Business(name: String)

  enum AtticType:
    case Empty
    case Commercial(business: Business)

  enum Floor:
    case ResidentialFloor(resident1: Resident, resident2: Resident, next: Option[Floor])
    case Attic(kind: AtticType)
    case Commercial(businesses: List[Business], next: Option[Floor])

  private def nextFloor(floor: Floor): Option[Floor] = floor match
    case Floor.ResidentialFloor(_, _, next) => next
    case Floor.Commercial(_, next)          => next
    case Floor.Attic(_)                     => None

  def fold[T](building: Building, accumulator: T)(f: (T, Floor) => T): T =
    @tailrec
    def loop(floor: Option[Floor], acc: T): T = floor match
      case Some(curr_floor) => loop(nextFloor(curr_floor), f(acc, curr_floor))
      case None             => acc

    loop(Some(building.firstFloor), accumulator)

  def countOldManFloors(building: Building, olderThan: Int): Int =
    fold(building, 0) { (count, floor) =>
      floor match
        case Floor.ResidentialFloor(r1, r2, _)
            if (r1.gender == Gender.Male && r1.age > olderThan) ||
              (r2.gender == Gender.Male && r2.age > olderThan) =>
          count + 1
        case _ => count
    }

  def womanMaxAge(building: Building): Option[Int] =
    fold(building, Option.empty[Int]) { (maxAge, floor) =>
      floor match
        case Floor.ResidentialFloor(r1, r2, _) =>
          val femaleAges = List(r1, r2).collect { case Resident(age, Gender.Female) => age }
          (maxAge ++ femaleAges).maxOption
        case _ => maxAge
    }

  def countCommercial(building: Building): Int =
    fold(building, 0) { (count, floor) =>
      floor match
        case Floor.Commercial(businesses, _)      => count + businesses.size
        case Floor.Attic(AtticType.Commercial(_)) => count + 1
        case _                                    => count
    }

def countCommercialAvg(buildings: List[Building]): Double =
  buildings match
    case Nil  => 0.0
    case list => list.map(Building.countCommercial).sum.toDouble / list.length
