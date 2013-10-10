
package arith

class Formatter {

  import Tree._
  import Tree.Op._
  import Tree.Expr._

  def format(t: Tree): String = t match {
    case ` + ` => "+"
    case ` - ` => "-"
    case ` * ` => "*"
    case ` / ` => "/"
    case o: Tree.Op => s"UnknownOp($o)"
    case Expr.Op(op, lhs, rhs) => s"${format(lhs)} ${format(rhs)} ${format(op)}"
    case Literal(d) => s"$d"
  }
}

