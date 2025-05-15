package ru.mipt.fp.service

import java.time.Instant
import cats.{Applicative, ApplicativeError}
import cats.syntax.all._
import ru.mipt.fp.domain.{AccountId, Card, CardOperationStats, NetworkError, OperationStats, Ucid}
import ru.mipt.fp.external.{CardsMasterSystemClient, OperationsSystemClient}
import ru.mipt.fp.resilience.{FallbackCache, Retry}

import scala.annotation.nowarn
import scala.concurrent.duration.FiniteDuration

@nowarn
class CardService[F[_]: Applicative](
  operationsSystemClient: OperationsSystemClient[F],
  cardsMasterSystemClient: CardsMasterSystemClient[F],
  retryCount: Int,
  retryDelay: FiniteDuration
)(using ApplicativeError[F, NetworkError], Retry[F, NetworkError], FallbackCache[F, Ucid, Card]):

  /** Запросить статистику по операциям по счету и выбранным картам за указанный период
    *
    * Статистику сгруппировать по доходным и расходным операциям
    *
    * Номер карты должен быть маскирован следующим образом: 1234-****-****-*789
    *
    * В кэше данные карт должны быть маскированы: номер карты как выше, вместо CVV "***" и вместо даты "**\**"
    *
    * Сетевые ошибки 5xx (Timeout, NotAvailable и InternalError) должны ретраиться, а ошибки 4хх (BadRequest и NotFound) - нет. Это нужно сделать с помощью Retry. 
    *
    * При исчерпании попыток должен возвращаться пустой список (Null Object pattern) для списков, и использоваться fallback cache для карт
    */
  def getOperationsStatistics(
    accountId: AccountId,
    cardIds: List[Ucid],
    from: Instant,
    to: Instant
  ): F[OperationStats] = {
    import ru.mipt.fp.masking.Masking.given

    val canRetry: NetworkError => Boolean = {
      case NetworkError.TimeoutError | NetworkError.NotAvailableError | NetworkError.InternalError => true
      case _                                                                                       => false
    }

    def getOperations[A](getOps: => F[List[A]]): F[List[A]] =
      summon[Retry[F, NetworkError]]
        .retry(getOps)(canRetry)(retryCount, retryDelay)
        .handleError(_ => List.empty)

    def getCardStats(ucid: Ucid): F[CardOperationStats] = {
      val opsF = getOperations(operationsSystemClient.getCardOperations(ucid, from, to))
        .map(_.map(_.amountChange))

      val cardF = summon[FallbackCache[F, Ucid, Card]]
        .withFallback(ucid) { id =>
          summon[Retry[F, NetworkError]]
            .retry(cardsMasterSystemClient.getCard(id))(canRetry)(retryCount, retryDelay)
            .map(_.masked)
        }
        .handleErrorWith { error =>
          summon[ApplicativeError[F, NetworkError]].raiseError(error)
        }

      (opsF, cardF).mapN { (ops, card) =>
        val (income, outcome) = ops.partition(_ > 0)
        CardOperationStats(
          card.number,
          income.sum,
          outcome.filter(_ < 0).sum
        )
      }
    }

    val accountOpsF = getOperations(operationsSystemClient.getAccountOperations(accountId, from, to))
      .map(_.map(_.amountChange))

    val cardStatsListF = cardIds.traverse(getCardStats)

    (accountOpsF, cardStatsListF).mapN { (accOps, cardStats) =>
      val (income, outcome) = accOps.partition(_ > 0)
      OperationStats(
        income.sum,
        outcome.filter(_ < 0).sum,
        cardStats
      )
    }
  }
