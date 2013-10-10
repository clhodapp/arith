
package arith

import collection.immutable.Stack
import java.util.Scanner

trait Source[T] { def getTokens(t: T): List[String] }

object Source {

  implicit object StringIsSource extends Source[String] {
    def getTokens(s: String) = """\s""".r.split(s).toList
  }

  implicit object ScannerIsSource extends Source[Scanner] {
    def getTokens(s: Scanner) =
      if (s.hasNext) s.next() :: getTokens(s)
      else Nil
  }

}

case class ParseException(message: String) extends Exception(message)

class Parser {

  import Tree.Expr._
  import Tree.Op._

  def parse[S: Source](s: S): Tree.Expr = {
    val tokens = implicitly[Source[S]].getTokens(s)
    def parse(
      remaining: List[String],
      stack: Stack[Tree.Expr]
    ): Tree.Expr = (remaining, stack) match {
      case (Nil, Stack(top)) => top
      case (OpChar(op) :: rest, s) if s.size >= 2 => parse(rest, s.drop(2).push(Op(op, s(1), s(0))))
      case (LitString(literal) :: rest, s) => parse(rest, s push Literal(literal))
      case _ => throw new ParseException(s"Invalid expression: ${tokens.mkString(" ")}")
    }
    parse(tokens, Stack.empty)
  }

  private[Parser] object LitString { def unapply(s: String) = util.Try { s.toDouble } toOption }

  private[Parser] object OpChar {
    def unapply(s: String) = s match {
      case "+" => Some(` + `)
      case "-" => Some(` - `)
      case "*" => Some(` * `)
      case "/" => Some(` / `)
      case _ => None
    }
  }
}


