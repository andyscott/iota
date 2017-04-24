/* -
 * Iota [iota-core]
 */

package iota
package internal

import cats._
import cats.data._
import cats.instances.all._

import scala.reflect.macros.whitebox.Context
import scala.reflect.macros.TypecheckException

final class CopKFunctionKMacros( val c: Context) {
  import c.universe._

  private[this] lazy val klists = new SharedKListMacros[c.type](c)

  def of[F[a] <: CopK[_, a], G[_]](args: c.Expr[Any]*)(
    implicit
      evF: c.WeakTypeTag[F[_]],
      evG: c.WeakTypeTag[G[_]]
  ): c.Expr[F ~> G] = {

    val F = symbol(evF.tpe)
    val G = symbol(evG.tpe)

    result(for {
      L    <- destructCopK(F).leftMap(NonEmptyList.of(_))
      tpes <- klists.klistTypesCached(L).leftMap(NonEmptyList.of(_))

      unorderedPairs <- Traverse[List].traverse(args.toList)(arg =>
        destructFunctionKInput(arg.tree.tpe, evG.tpe.dealias).map((_, arg.tree))).toEither
      lookup = unorderedPairs.toMap

      arrs <- Traverse[List].traverse(tpes)(tpe =>
        lookup.get(tpe).toRight(s"Missing interpreter FunctionK[$tpe, $G]").toValidatedNel).toEither
    } yield makeInterpreter(F, L, G, arrs))
  }

  def summon[F[a] <: CopK[_, a], G[_]](
    implicit
      evF: c.WeakTypeTag[F[_]],
      evG: c.WeakTypeTag[G[_]]
  ): c.Expr[F ~> G] = {

    val F = symbol(evF.tpe)
    val G = symbol(evG.tpe)

    result(for {
      L    <- destructCopK(F).leftMap(NonEmptyList.of(_))
      tpes <- klists.klistTypesCached(L).leftMap(NonEmptyList.of(_))

      arrs <- Traverse[List].traverse(tpes)(tpe =>
                summonFunctionK(tpe, G)).toEither
    } yield makeInterpreter(F, L, G, arrs))
  }

  private[this] def symbol(tpe: Type): Symbol =
    tpe match {
      case TypeRef(_, sym, Nil) => sym
      case _                    => tpe.typeSymbol
    }

  private[this] def makeInterpreter(
    F: Symbol,
    L: Type,
    G: Symbol,
    arrs: List[Tree]
  ): Tree = {

    val namedArrs = arrs.zipWithIndex.map { case (arr, i) =>
      (TermName(s"arr$i"), arr, i) }

    val defs = namedArrs.map { case (n, arr, _) =>
      q"private[this] def $n = $arr.asInstanceOf[cats.arrow.FunctionK[Any, $G]]" }

    val cases = namedArrs.map { case (n, _, i) =>
      cq"$i => $n(ca.value)" }

    q"""
    new cats.arrow.FunctionK[$F, $G] {
      ..$defs
      override def apply[A](ca: $F[A]): $G[A] =
        (ca.index: @scala.annotation.switch) match {
          case ..$cases
        }
    }
    """
  }

  private[this] def summonFunctionK(F: Type, G: Symbol): ValidatedNel[String, Tree] =
    Validated
      .catchOnly[TypecheckException](
        c.typecheck(q"scala.Predef.implicitly[cats.arrow.FunctionK[$F, $G]]"))
      .leftMap(t => NonEmptyList.of(t.msg))

  private[this] def destructCopK(F: Symbol): Either[String, Type] =
    F.asType.toType.dealias match {
      case TypeRef(_, _, t :: _ :: Nil) => Right(t)
      case t => Left(s"unespected type $t in destructured $F")
    }


  private[this] def destructFunctionKInput(tpe: Type, G: Type): ValidatedNel[String, Type] =
    tpe match {
      case TypeRef(_, sym, f :: g :: Nil) if g =:= G => Validated.valid(f)
      case RefinedType(anyRef :: tpe2 :: Nil, scope) => // TODO: check anyRef is scala.AnyRef
        destructFunctionKInput(tpe2.dealias, G)
      case _ =>
        Validated.invalidNel(s"unable to destruct input $tpe as FunctionK[?, $G]\n" +
          s"  underlying type tree: ${showRaw{tpe}} (class ${tpe.getClass})")
    }

  private[this] def result[T](either: Either[NonEmptyList[String], Tree]): c.Expr[T] =
    either fold (
      errors => c.abort(c.enclosingPosition, errors.toList.mkString(", and\n")),
      tree   => c.Expr[T](tree))
}
