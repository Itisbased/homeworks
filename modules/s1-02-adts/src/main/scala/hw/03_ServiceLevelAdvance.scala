package hw

/** III. Сервис лояльности
  *
  * Разрабатываем сервис лояльности/уровней обслуживания для клиентов компании.
  * Уровни лояльности реализованы с помощью иерархии классов, начиная с самого
  * простого Economy (родительский класс для всех остальных) Чем ниже мы
  * продвигаемся по потомкам от Economy тем круче класс. ServiceLevelAdvance
  * необходим для продвижения клиента от менее крутых к более крутым уровням.
  */
class Economy
class UpgradedEconomy extends Economy
class Special1b extends UpgradedEconomy
class ExtendedEconomy extends Economy
class Business extends ExtendedEconomy
class Elite extends Business
class Platinum extends Business

/** Модифицируйте код таким образом, чтобы ServiceLevelAdvance был привязан к
  * уровню обслуживания, а ServiceLevelAdvance.advance мог только повышать
  * уровень обслуживания. Подсказку, как именно модифицировать, вы найдете в
  * тестах.
  */
trait LevelUpgrade[L, H]

object LevelUpgrade:
  given LevelUpgrade[Economy, UpgradedEconomy] = new LevelUpgrade[Economy, UpgradedEconomy] {}
  given LevelUpgrade[UpgradedEconomy, Special1b] = new LevelUpgrade[UpgradedEconomy, Special1b] {}
  given LevelUpgrade[Economy, ExtendedEconomy] = new LevelUpgrade[Economy, ExtendedEconomy] {}
  given LevelUpgrade[ExtendedEconomy, Business] = new LevelUpgrade[ExtendedEconomy, Business] {}
  given LevelUpgrade[Business, Elite] = new LevelUpgrade[Business, Elite] {}
  given LevelUpgrade[Business, Platinum] = new LevelUpgrade[Business, Platinum] {}

class ServiceLevelAdvance[L]:
  def advance[H](using LevelUpgrade[L, H]): ServiceLevelAdvance[H] =
    new ServiceLevelAdvance[H]
