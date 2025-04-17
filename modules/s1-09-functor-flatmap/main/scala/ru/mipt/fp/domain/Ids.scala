package ru.mipt.fp.domain

// /*
import ru.mipt.fp.masking.Masking
// */

case class Ucid(ucid: String) extends AnyVal

case class ClientId(clientId: String) extends AnyVal

case class CardNumber(cardNumber: String) extends AnyVal

// /*
object CardNumber:
  given Masking[CardNumber] with
    def mask(cardNumber: CardNumber): CardNumber =
      CardNumber(cardNumber.cardNumber.take(4) + "-****-****-*" + cardNumber.cardNumber.takeRight(3))

  // */

case class CardCvv(cardCvv: String) extends AnyVal

// /*
object CardCvv:
  given Masking[CardCvv] with
    def mask(cardCvv: CardCvv): CardCvv = CardCvv("***")

  // */

case class CardExpirationDate(cardExpirationDate: String) extends AnyVal

// /*
object CardExpirationDate:
  given Masking[CardExpirationDate] with
    def mask(cardExpirationDate: CardExpirationDate): CardExpirationDate = CardExpirationDate("**\\**")

  // */
