package ch10.monad

trait Monad[F[_]]:
  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  // map implemented in terms of pure and flatMap
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))

object Monad:
  def apply[F[_]](using m: Monad[F]): Monad[F] = m

  extension [A](a: A) 
    def puree[F[_]: Monad]: F[A] = Monad[F].pure[A](a)

  given idMonad: Monad[Id] with
    def pure[A](a: A): Id[A]                           = a
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
