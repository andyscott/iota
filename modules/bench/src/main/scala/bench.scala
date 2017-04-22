/* -
 * Iota [iota-bench]
 */

package iota_bench

import iota._

import cats._
import cats.data._
import cats.free._


object CatsStyle {
  import Ops._

  type AlgebraA[A] = AOp[A]
  type AlgebraB[A] = Coproduct[BOp, AlgebraA, A]
  type AlgebraC[A] = Coproduct[COp, AlgebraB, A]
  type AlgebraD[A] = Coproduct[DOp, AlgebraC, A]
  type AlgebraE[A] = Coproduct[EOp, AlgebraD, A]

  val evalA: AlgebraA ~> Id = AOp.eval
  val evalB: AlgebraB ~> Id = BOp.eval or evalA
  val evalC: AlgebraC ~> Id = COp.eval or evalB
  val evalD: AlgebraD ~> Id = DOp.eval or evalC
  val evalE: AlgebraE ~> Id = EOp.eval or evalD
}

object IotaStyle {
  import Ops._

  import KList.::
  type AlgebraA[A] = CopK[AOp :: KNil, A]
  type AlgebraB[A] = CopK[BOp :: AOp :: KNil, A]
  type AlgebraC[A] = CopK[COp :: BOp :: AOp :: KNil, A]
  type AlgebraD[A] = CopK[DOp :: COp :: BOp :: AOp :: KNil, A]
  type AlgebraE[A] = CopK[EOp :: DOp :: COp :: BOp :: AOp :: KNil, A]


  val evalA: AlgebraA ~> Id = CopK.FunctionK.of[AlgebraA, Id](AOp.eval)
  val evalB: AlgebraB ~> Id = CopK.FunctionK.of(AOp.eval, BOp.eval)
  val evalC: AlgebraC ~> Id = CopK.FunctionK.of(AOp.eval, BOp.eval, COp.eval)
  val evalD: AlgebraD ~> Id = CopK.FunctionK.of(AOp.eval, BOp.eval, COp.eval, DOp.eval)
  val evalE: AlgebraE ~> Id = CopK.FunctionK.of(AOp.eval, BOp.eval, COp.eval, DOp.eval, EOp.eval)
}

object Bench extends App {
  import Ops._

  def timeEvaluation[F[_]](eval: F ~> Id): Unit = {
    val v: F[_] = null.asInstanceOf[F[_]]
    val r: Id[_] = eval(v)
  }

  {
    import CatsStyle._
    timeEvaluation(evalA)
    timeEvaluation(evalB)
    timeEvaluation(evalC)
    timeEvaluation(evalD)
    timeEvaluation(evalE)
  }

  {
    import IotaStyle._
    timeEvaluation(evalA)
    timeEvaluation(evalB)
    timeEvaluation(evalC)
    timeEvaluation(evalD)
    timeEvaluation(evalE)
  }

}


object GenStuff extends App {
  import scala.Predef._

  def catsAlgebraHeadTemplate(
    name: String
  ): String =
    s"type Algebra$name[A] = ${name}Op[A]"

  def catsAlgebraTailTemplate(
    prevName: String,
    name: String
  ): String =
    s"type Algebra$name[A] = Coproduct[${name}Op, Algebra$prevName, A]"

  def catsEvalHeadTemplate(
    name: String
  ): String =
    s"val eval${name}: Algebra${name} ~> Id = ${name}Op.eval"

  def catsEvalTailTemplate(
    prevName: String,
    name: String
  ): String =
    s"val eval$name: AlgebraB ~> Id = BOp.eval or evalA"

  def opsTemplate(
    name: String
  ): String = s"""
    |  sealed trait ${name}Op[A]
    |  object ${name}Op {
    |    case class Op1(v: Int) extends ${name}Op[Int]
    |    lazy val eval = λ[${name}Op ~> Id] { case ${name}Op.Op1(v) => v + 1 }
    |  }
    |  class ${name}Ops[F[_]](implicit inj: InjK[${name}Op, F]) {
    |    def op1(v: Int): Free[F, Int] = Free.liftF(inj(${name}Op.Op1(v)))
    |  }""".stripMargin

  val alphabet = List(
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
    "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")

  alphabet.take(5).foreach { l =>
    println(template(l.toUpperCase))
  }

}

object Ops {

  sealed trait AOp[A]
  object AOp {
    case class Op1(v: Int) extends AOp[Int]
    lazy val eval = λ[AOp ~> Id] { case AOp.Op1(v) => v + 1 }
  }
  class AOps[F[_]](implicit inj: InjK[AOp, F]) {
    def op1(v: Int): Free[F, Int] = Free.liftF(inj(AOp.Op1(v)))
  }

  sealed trait BOp[A]
  object BOp {
    case class Op1(v: Int) extends BOp[Int]
    lazy val eval = λ[BOp ~> Id] { case BOp.Op1(v) => v + 1 }
  }
  class BOps[F[_]](implicit inj: InjK[BOp, F]) {
    def op1(v: Int): Free[F, Int] = Free.liftF(inj(BOp.Op1(v)))
  }

  sealed trait COp[A]
  object COp {
    case class Op1(v: Int) extends COp[Int]
    lazy val eval = λ[COp ~> Id] { case COp.Op1(v) => v + 1 }
  }
  class COps[F[_]](implicit inj: InjK[COp, F]) {
    def op1(v: Int): Free[F, Int] = Free.liftF(inj(COp.Op1(v)))
  }

  sealed trait DOp[A]
  object DOp {
    case class Op1(v: Int) extends DOp[Int]
    lazy val eval = λ[DOp ~> Id] { case DOp.Op1(v) => v + 1 }
  }
  class DOps[F[_]](implicit inj: InjK[DOp, F]) {
    def op1(v: Int): Free[F, Int] = Free.liftF(inj(DOp.Op1(v)))
  }

  sealed trait EOp[A]
  object EOp {
    case class Op1(v: Int) extends EOp[Int]
    lazy val eval = λ[EOp ~> Id] { case EOp.Op1(v) => v + 1 }
  }
  class EOps[F[_]](implicit inj: InjK[EOp, F]) {
    def op1(v: Int): Free[F, Int] = Free.liftF(inj(EOp.Op1(v)))
  }

}
