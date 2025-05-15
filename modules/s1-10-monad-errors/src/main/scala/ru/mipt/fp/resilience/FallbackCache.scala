package ru.mipt.fp.resilience

import cats.MonadError
import cats.syntax.all._
import ru.mipt.fp.utils.Cache
import ru.mipt.fp.domain.NetworkError

import scala.annotation.nowarn

/**
 * Интерфейс для фоллбэк-кэша
 */
trait FallbackCache[F[_], K, V]:
  def withFallback(key: K)(operation: K => F[V]): F[V]

object FallbackCache:
  @nowarn
  private class Impl[F[_], K, V](cache: Cache[F, K, V])(using F: MonadError[F, NetworkError])
    extends FallbackCache[F, K, V]:
    def withFallback(key: K)(operation: K => F[V]): F[V] =
      operation(key)
        .flatTap(v => cache.put(key, v))
        .handleErrorWith(e =>
          cache.get(key).flatMap {
            case Some(v) => F.pure(v)
            case None    => F.raiseError(e)
          }
        )

  def apply[F[_], K, V](cache: Cache[F, K, V])(using F: MonadError[F, NetworkError]): FallbackCache[F, K, V] =
    new Impl(cache)
