package ru.mipt.fp.masking

import ru.mipt.fp.domain._

/** Тайпкласс для маскирования данных
  */
trait Masking[T]:
  def mask(t: T): T

  extension (t: T) def masked: T = mask(t)

object Masking:
  given Masking[CardNumber] with
    def mask(t: CardNumber): CardNumber =
      val orig = t.cardNumber
      val masked = orig match
        case s if s.length == 19 && s.count(_ == '-') == 3 =>
          s.take(4) + "-****-****-*" + s.takeRight(3)
        case s => s
      CardNumber(masked)

  given Masking[CardCvv] with
    def mask(t: CardCvv): CardCvv = CardCvv("***")

  given Masking[CardExpirationDate] with
    def mask(t: CardExpirationDate): CardExpirationDate = CardExpirationDate("**\\**")

  given Masking[Card] with
    def mask(t: Card): Card =
      Card(
        t.ucid,
        t.number.masked,
        t.cvv.masked,
        t.expirationDate.masked
      )
