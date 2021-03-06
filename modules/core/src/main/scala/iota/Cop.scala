/* -
 * Iota [iota-core]
 */

package iota

/** A coproduct of types captured by type list `L` */
sealed abstract class Cop[L <: TList] {
  type Algebras = L
}

object Cop {

  /** The single value of the coproduct stored with the index of its type
    * in the type list `L`
    */
  final case class Value[L <: TList, A] private[iota](index: Int, value: A) extends Cop[L]

  /** A type class witnessing the ability to inject type `A` into a coproduct of
    * types `L`
    */
  final class Inject[A, L <: TList] private[Inject](index: Int) {
    def inj(a: A): Cop[L] = Cop.Value[L, A](index, a)
    def proj(ca: Cop[L]): Option[A] = ca match {
      case Cop.Value(i, v: A @unchecked) if i == index => Some(v)
      case _ => None
    }
  }

  object Inject {
    def apply[A, L <: TList](implicit ev: Inject[A, L]): Inject[A, L] = ev
    implicit def makeInject[A, L <: TList](implicit ev: TList.Pos[L, A]): Inject[A, L] =
      new Inject[A, L](ev.index)
  }
}
