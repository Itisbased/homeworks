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
class SchoolClass(collection: Seq[KnowNothing]):
  def accept(students: Seq[KnowNothing]): SchoolClass = new SchoolClass(collection ++ students)
