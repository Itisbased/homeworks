package ru.mipt.fp.utils

/**
 * Интерфейс кэша для фоллбэков
 */
trait Cache[F[_], K, V]:
  def get(key: K): F[Option[V]]
  def put(key: K, value: V): F[Unit]
