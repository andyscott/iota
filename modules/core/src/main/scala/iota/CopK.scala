/* -
 * Iota [iota-core]
 */

package iota

import scala.Predef.=:=

import cats.free._

/** A coproduct of type constructors captured by type constructor list `L` */
final class CopK[L <: KList, A] private[iota](
  val index: Int,
  val value: Any
) {
  type Algebras = L

  override def equals(anyOther: Any): Boolean = anyOther match {
    case other: CopK[L, A] => (index == other.index) && (value == other.value)
    case _                 => false
  }

  override def toString: String =
    s"CopK($value @ $index)"
}

object CopK {

  def apply[L <: KList, F[_], A](index: Int, fa: F[A]): CopK[L, A] =
    new CopK[L, A](index, fa)

  /** A type class witnessing the ability to inject type constructor `F`
    * into a coproduct of types constructors `L`
    */
  final class Inject[F[_], L <: KList] private[Inject](index: Int) {
    def inj[A](fa: F[A]): CopK[L, A] = new CopK[L, A](index, fa)
    def proj[A](ca: CopK[L, A]): Option[F[A]] =
      if (ca.index == index) Some(ca.value.asInstanceOf[F[A]])
      else None
  }

  object Inject {
    def apply[F[_], L <: KList](implicit ev: Inject[F, L]): Inject[F, L] = ev
    implicit def makeInject[F[_], L <: KList](implicit ev: KList.Pos[L, F]): Inject[F, L] =
      new Inject[F, L](ev.index)
  }

  def liftFree[G[_]]: LiftFreePartial[G] = new LiftFreePartial[G]

  final class LiftFreePartial[G[_]] private[CopK] {
    def apply[F[_], A, L <: KList](fa: F[A])(
      implicit ev: CopK[L, A] =:= G[A], I: CopK.Inject[F, L]
    ): Free[G, A] = Free.liftF(ev(I.inj(fa)))
  }

  val FunctionK: CopKFunctionK.type = CopKFunctionK
}
