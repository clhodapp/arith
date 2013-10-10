
package arith

sealed trait Tree

object Tree {

  sealed trait Expr extends Tree

  object Expr {

    case class Op(op: Tree.Op, lsh: Expr, rhs: Expr) extends Expr

    case class Literal(d: Double) extends Expr

  }

  sealed trait Op extends Tree

  object Op extends Op {

    case object ` + ` extends Op
    case object ` - ` extends Op
    case object ` * ` extends Op
    case object ` / ` extends Op

  }

}

