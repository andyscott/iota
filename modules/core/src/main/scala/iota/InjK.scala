/* -
 * Iota [iota-core]
 */

package iota

import cats._
import cats.arrow.FunctionK
import cats.free.Inject

sealed abstract class InjK[F[_], G[_]] {
  def inj: FunctionK[F, G]
  def prj: FunctionK[G, λ[α => Option[F[α]]]]
  final def apply[A](fa: F[A]): G[A] = inj(fa)
  final def unapply[A](ga: G[A]): Option[F[A]] = prj(ga)
}

object InjK {
  implicit def injKFromCatsInject[F[_], G[_]](
    implicit ev: Inject[F, G]
  ): InjK[F, G] = new InjK[F, G] {
    def inj = λ[F ~> G](ev.inj(_))
    def prj = λ[G ~> λ[α => Option[F[α]]]](ev.prj(_))
  }

  implicit def injKfromCopKInj[F[_], L <: KList](
    implicit ev: CopK.Inject[F, L]
  ): InjK[F, CopK[L, ?]] = new InjK[F, CopK[L, ?]] {
    def inj = λ[F ~> CopK[L, ?]](ev.inj(_))
    def prj = λ[CopK[L, ?] ~> λ[α => Option[F[α]]]](ev.proj(_))
  }
}
