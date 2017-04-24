/* -
 * Iota [iota-core]
 */

package iota

/** A coproduct of types captured by type list `L` */
final class Cop[L <: TList] private[iota](
  val index: Int,
  val value: Any
) {
  type Algebras = L

  override def equals(anyOther: Any): Boolean = anyOther match {
    case other: Cop[L] => (index == other.index) && (value == other.value)
    case _             => false
  }

  override def toString: String =
    s"Cop($value @ $index)"
}

object Cop {

  def apply[L <: TList, A](index: Int, a: A): Cop[L] =
    new Cop[L](index, a)

  /** A type class witnessing the ability to inject type `A` into a coproduct of
    * types `L`
    */
  final class Inject[A, L <: TList] private[Inject](index: Int) {
    def inj(a: A): Cop[L] = new Cop[L](index, a)
    def proj(ca: Cop[L]): Option[A] =
      if (ca.index == index) Some(ca.value.asInstanceOf[A])
      else None
  }

  object Inject {
    def apply[A, L <: TList](implicit ev: Inject[A, L]): Inject[A, L] = ev
    implicit def makeInject[A, L <: TList](implicit ev: TList.Pos[L, A]): Inject[A, L] =
      new Inject[A, L](ev.index)
  }
}
