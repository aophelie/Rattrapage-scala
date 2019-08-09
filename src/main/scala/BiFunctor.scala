import simulacrum.typeclass

trait Bifunctor[F[_, _]] { self =>

  def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]

  def rightFunctor[X]: Functor[F[X, *]] =
    new RightFunctor[F, X] { val F = self }

  def leftFunctor[X]: Functor[F[*, X]] =
    new LeftFunctor[F, X] { val F = self }


  def leftMap[A, B, C](fab: F[A, B])(f: A => C): F[C, B] = bimap(fab)(f, identity)

  /** The composition of two Bifunctors is itself a Bifunctor */
  def compose[G[_, _]](implicit G0: Bifunctor[G]): Bifunctor[λ[(α, β) => F[G[α, β], G[α, β]]]] =
    new ComposedBifunctor[F, G] {
      val F = self
      val G = G0
    }


  def leftWiden[A, B, AA >: A](fab: F[A, B]): F[AA, B] = fab.asInstanceOf[F[AA, B]]
}