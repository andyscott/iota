/* -
 * Iota [iota-bench-run]
 */

package iota_bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class Bench {

}

import cats.free._

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
