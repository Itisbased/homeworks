package ru.mipt.fp.resilience

import cats.MonadError
import ru.mipt.fp.utils.Timer

import scala.concurrent.duration.FiniteDuration

trait Retry[F[_], E]:
  def retry[A](operation: F[A])(canRetry: E => Boolean)(count: Int, delay: FiniteDuration): F[A]

object Retry:
  private class Impl[F[_]: Timer, E](using
    MonadError[F, E]
  ) extends Retry[F, E]:
    override def retry[A](
      operation: F[A]
    )(canRetry: E => Boolean)(count: Int, delay: FiniteDuration): F[A] =
      val F = summon[MonadError[F, E]]
      val timer = summon[Timer[F]]

      def retryLoop(remainingAttempts: Int): F[A] =
        F.handleErrorWith(operation) { error =>
          if remainingAttempts > 0 && canRetry(error) then
            F.flatMap(timer.sleep(delay))(_ => retryLoop(remainingAttempts - 1))
          else F.raiseError(error)
        }

      retryLoop(count)

  def apply[F[_]: Timer, E](using
    MonadError[F, E]
  ): Retry[F, E] = new Impl[F, E]
