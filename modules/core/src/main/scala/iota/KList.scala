/* -
 * Iota [iota-core]
 */

package iota

/** A heterogenous list of type constructors */
trait KList

object KList {

  /** A syntactic sugar alias for [[KCons]] */
  type ::[H[_], T <: KList] = KCons[H, T]

  /** A syntactic sugar alias for [[KCons]] */
  type :::[H[_], T <: KList] = KCons[H, T]

  /** A type class that witnesses the position of type constructor `F` in type
    * constructor list `L`
    */
  trait Pos[L <: KList, F[_]] {
    def index: Int
  }

  object Pos {
    def apply[L <: KList, F[_]](implicit ev: Pos[L, F]): Pos[L, F] = ev
    implicit def materializePos[L <: KList, F[_]]: Pos[L, F] =
      macro internal.KListMacros.materializePos[L, F]
  }

  /** A type class that witnesses the type constructor at a given index in a
    * type constructor list
    *
    * @tparam L the type constructor list
    * @tparam I the singleton index type
    */
  trait AtPos[L <: KList, I <: Int] {
    type Out[A]
  }

  object AtPos {
    type Aux[L <: KList, I <: Int, F[_]] = AtPos[L, I] { type Out[A] = F[A] }
    def apply[L <: KList, I <: Int](implicit ev: AtPos[L, I]): AtPos.Aux[L, I, ev.Out] = ev
    implicit def materializeAtPos[L <: KList, I <: Int, F[_]]: AtPos.Aux[L, I, F] =
      macro internal.KListMacros.materializeAtPos[L, I, F]
  }
}
