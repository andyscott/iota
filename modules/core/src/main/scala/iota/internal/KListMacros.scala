/* -
 * Iota [iota-core]
 */

package iota
package internal

import scala.unchecked
import scala.reflect.macros.whitebox.Context
import scala.reflect.ClassTag
import scala.annotation.tailrec

import scala.collection.immutable.Map

class KListMacros(val c: Context) {
  import c.universe._

  val klists = new SharedKListMacros[c.type](c)

  def materializePos[L <: KList, F[_]](
    implicit
      evL: c.WeakTypeTag[L],
      evF: c.WeakTypeTag[F[_]]
  ): c.Expr[KList.Pos[L, F]] = {

    val L = evL.tpe.dealias
    val F = evF.tpe.dealias

    result(for {
      algebras <- klists.klistTypesCached(L)
      index    <- Right(algebras.indexWhere(_.dealias == F))
                    .filterOrElse(_ >= 0, s"$F is not a member of $L")
    } yield
      q"new KList.Pos[$L, $F]{ override val index: Int = $index }")
  }

  def materializeAtPos[L <: KList, I <: Int, F[_]](
    implicit
      evL: c.WeakTypeTag[L],
      evI: c.WeakTypeTag[I]
  ): c.Expr[KList.AtPos.Aux[L, I, F]] = {

    val L = evL.tpe.dealias
    val I = evI.tpe.dealias

    result(for {
      algebras <- klists.klistTypesCached(L)
      index    <- singletonTypeValue[Int](I)
      tpe      <- algebras.lift(index).toRight(s"index $index out of bounds for type list $algebras")
    } yield
      q"new KList.AtPos[$L, $I] { type Out[A] = ${tpe.typeSymbol}[A] }")
  }

  private[this] def singletonTypeValue[T](tpe: Type)(
    implicit T: ClassTag[T]
  ): Either[String, T] = tpe match {
    case ConstantType(Constant(t: T)) => Right(t)
    case _ => Left(s"$tpe is not a singleton of type $T")
  }

  def result[T](either: Either[String, Tree]): c.Expr[T] =
    either fold (
      error => c.abort(c.enclosingPosition, error),
      tree  => c.Expr[T](tree))

}

private[internal] object SharedKListMacros {
  @volatile private[SharedKListMacros] var klistCache: Map[Any, Any] = Map.empty
}

private[internal] class SharedKListMacros[C <: Context](val c: C) {
  import c.universe._

  private[this] val KNilSym          = typeOf[KNil].typeSymbol
  private[this] val KConsSym         = typeOf[KCons[Nothing, Nothing]].typeSymbol

  @tailrec
  private[this] final def klistFoldLeft[A](tpe: Type)(a0: A)(f: (A, Type) => A): Either[String, A] = tpe match {
    case TypeRef(_, KNilSym, Nil) => Right(a0)
    case TypeRef(_, cons, List(headType, tailType)) if cons.asType.toType.contains(KConsSym) =>
      klistFoldLeft(tailType)(f(a0, headType))(f)
    case _ =>
      Left(s"Unexpected type ${showRaw(tpe)} when inspecting KList")
  }

  private[this] final def klistTypes(tpe: Type): Either[String, List[Type]] =
    klistFoldLeft(tpe)(List.empty[Type])((acc, t) => t :: acc).map(_.reverse)

  final def klistTypesCached(
    tpe: Type
  ): Either[String, List[Type]] = SharedKListMacros.klistCache.synchronized {
    SharedKListMacros.klistCache.get(tpe) match {
      case Some(res: Either[String, List[Type]] @unchecked) =>
        res
      case _ =>
        val res = klistTypes(tpe)
        SharedKListMacros.klistCache += ((tpe, res))
        res
    }
  }

}
