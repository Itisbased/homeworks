package hw

/** II. Школьный класс
  *
  * Вы директор среднеобразовательной школы и как раз сейчас занимаетесь набором
  * учеников в новый класс. К сожалению, далеко не все ученики одинаково хороши.
  * Есть дети с разным уровнем знаний и характером, начиная от незнаек
  * KnowNothing и заканчивая гениями Genius. Связи уровней реализованы в виде
  * иерархии представленной ниже:
  */
class KnowNothing
class Aggressive extends KnowNothing
class KnowSomething extends KnowNothing
class PoorlyEducated extends KnowSomething
class Normal extends PoorlyEducated
class Enlightened extends Normal
class Genius extends Enlightened

/** Рассмотрим класс SchoolClass:
  *
  * Вам необходимо его дописать/переписать для приема новичков, чтобы
  * SchoolClass имел привязку к знаниям своих учеников. Подсказку, как именно
  * переписать, вы найдете в тестах. При этом будем считать что класс в целом
  * отражает худших своих учеников и метод accept должен возвращать новый
  * SchoolClass с наименьшими знаниями/уровнем всех его учеников. После
  * реализации класса проверить его на примере — начать класс с гения и
  * "скатиться" до KnowNothing
  */

class SchoolClass[T <: KnowNothing](val collection: Seq[T]):
  def accept[S <: KnowNothing](students: Seq[S]): SchoolClass[Min[T, S]] =
    new SchoolClass[Min[T, S]](collection ++ students)

type Min[A <: KnowNothing, B <: KnowNothing] <: KnowNothing = A match
  case Genius => B
  case Enlightened => B match
    case Genius => A
    case _ => B
  case Normal => B match
    case Genius | Enlightened => A
    case _ => B
  case PoorlyEducated => B match
    case Genius | Enlightened | Normal => A
    case _ => B
  case KnowSomething => B match
    case Genius | Enlightened | Normal | PoorlyEducated => A
    case _ => B
  case Aggressive => B match
    case Genius | Enlightened | Normal | PoorlyEducated | KnowSomething => A
    case _ => B
  case KnowNothing => KnowNothing