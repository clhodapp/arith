
package arith

case class EvaluationException(message: String) extends Exception(message)

class Interpreter {

  import Tree.Op._
  import Tree.Expr._

  def eval(t: Tree.Expr): Double = t match {
    case Literal(d) => d
    case Op(` + `, lhs, rhs) => eval(lhs) + eval(rhs)
    case Op(` - `, lhs, rhs) => eval(lhs) - eval(rhs)
    case Op(` * `, lhs, rhs) => eval(lhs) * eval(rhs)
    case Op(` / `, lhs, rhs) => eval(lhs) / eval(rhs)
    case Op(op, lhs, rhs) => throw new EvaluationException(s"Bad Operator: $op")
    case t => throw new EvaluationException(s"Bad tree: $t")
  }

}

