package ru.mipt.fp.service

import ru.mipt.fp.cache.Cache
import ru.mipt.fp.domain.{Card, ClientId, Ucid, CardNumber, CardCvv, CardExpirationDate}
import ru.mipt.fp.external.CardsMasterSystemClient

import scala.annotation.nowarn
import scala.concurrent.duration.FiniteDuration
import cats.Applicative

@nowarn
class CardService[F[_]](
  cardsClient: CardsMasterSystemClient[F],
  cardsCache: Cache[F, Ucid, Card],
  cardsListCache: Cache[F, ClientId, List[Card]],
  cacheTtl: FiniteDuration
)(using F: cats.FlatMap[F], A: Applicative[F]):

  /** Запросить данные всех карт текущего пользователя, маскировать чувствительные данные и вернуть весь список
    *
    * Номер карты из 4444-4444-4444-4567 должен стать 4444-****-****-*567
    *
    * Срок действия и CVV должны замениться на '**\**' и *** соответственно
    *
    * Чтение должно быть кэширующим Из соображений безопасности в кэше не должно быть немаскированных данных по карте
    *
    * Кэширование должно производиться с TTL, указанным в конструкторе
    *
    * При чтении данных из кэша время жизни должно продлеваться
    */
  def getClientCards(clientId: ClientId): F[List[Card]] =
    F.flatMap(cardsListCache.get(clientId)) {
      case Some(cards) =>
        F.flatMap(cardsListCache.expire(clientId, cacheTtl))(_ => A.pure(cards))
      case None =>
        F.flatMap(cardsClient.getClientCards(clientId)) { cards =>
          val maskedCards = cards.map(maskCard)
          F.flatMap(cardsListCache.put(clientId, maskedCards))(_ =>
            F.flatMap(cardsListCache.expire(clientId, cacheTtl))(_ => A.pure(maskedCards))
          )
        }
    }

  /** Запросить данные карты из внешнего хранилища по ее Ucid, выполнить маскирование и вернуть информацию
    *
    * Номер карты из 4444-4444-4444-4567 должен стать 4444-****-****-*567
    *
    * Срок действия и CVV должны замениться на **\** и *** соответственно
    *
    * Чтение должно быть кэширующим Из соображений безопасности в кэше не должно быть немаскированных данных по карте
    *
    * Кэширование должно производиться с TTL, указанным в конструкторе
    *
    * При чтении данных из кэша время жизни должно продлеваться
    */
  def getCardById(ucid: Ucid): F[Card] =
    F.flatMap(cardsCache.get(ucid)) {
      case Some(card) =>
        F.flatMap(cardsCache.expire(ucid, cacheTtl))(_ => A.pure(card))
      case None =>
        F.flatMap(cardsClient.getCard(ucid)) { card =>
          val maskedCard = maskCard(card)
          F.flatMap(cardsCache.put(ucid, maskedCard))(_ =>
            F.flatMap(cardsCache.expire(ucid, cacheTtl))(_ => A.pure(maskedCard))
          )
        }
    }

  /** Запросить деактивацию карты по ее Ucid
    *
    * Запрос должен инвалидировать кэш для этой карты
    */
  def deactivateCard(ucid: Ucid): F[Unit] =
    F.flatMap(cardsClient.deactivateCard(ucid))(_ => cardsCache.invalidate(ucid))

  private def maskCard(card: Card): Card = {
    val numberStr = card.number.cardNumber
    val maskedNumber = numberStr.take(4) + "-****-****-*" + numberStr.takeRight(3)
    val maskedCvv = "***"
    val maskedExpiration = "**\\**"

    card.copy(
      number = CardNumber(maskedNumber),
      cvv = CardCvv(maskedCvv),
      expirationDate = CardExpirationDate(maskedExpiration)
    )
  }
