/* -
 * Iota [iota-core]
 */

package iota

/** A heterogenous list of types */
trait TList

object TList {

  /** A syntactic sugar alias for [[TCons]] */
  type ::[H, T <: TList] = TCons[H, T]

  /** A syntactic sugar alias for [[TCons]] */
  type :::[H, T <: TList] = TCons[H, T]

  /** A type class that witnesses the position of type `A` in type
    * list `L`
    */
  trait Pos[L <: TList, A] {
    def index: Int
  }

  object Pos {
    def apply[L <: TList, A](implicit ev: Pos[L, A]): Pos[L, A] = ev
    implicit def materializePos[L <: TList, A]: Pos[L, A] =
      macro internal.TListMacros.materializePos[L, A]
  }

  /** A type class that witnesses the type at a given index in a
    * type list
    *
    * @tparam L the type list
    * @tparam I the singleton index type
    */
  trait AtPos[L <: TList, I <: Int] {
    type Out
  }

  object AtPos {
    type Aux[L <: TList, I <: Int, A] = AtPos[L, I] { type Out = A }
    def apply[L <: TList, I <: Int](implicit ev: AtPos[L, I]): AtPos.Aux[L, I, ev.Out] = ev
    implicit def materializeAtPos[L <: TList, I <: Int, A]: AtPos.Aux[L, I, A] =
      macro internal.TListMacros.materializeAtPos[L, I, A]
  }
}
