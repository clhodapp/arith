
package arith

import org.rogach.scallop._

import jline._

import java.io.EOFException
import java.io.File
import java.util.Scanner

class Conf(args: Seq[String]) extends ScallopConf(args) {
  val interactive = toggle()
  val file = trailArg[String](required = false)
  mutuallyExclusive(interactive, file)
  validateOpt(interactive, file) { (i, f) =>
    if (i.isEmpty && f.isEmpty) Left("No input specified")
    else Right()
  }
}

object Arith extends App {

  val i = new Interpreter
  val p = new Parser
  val reader = new ConsoleReader

  val conf = new Conf(args)

  private[Arith] implicit class Function1HasLift[A, B](val f: A => B) extends AnyVal {
    def lift: Option[A] => Option[B] = (a: Option[A]) => a.map(f)
  }


  def repl {

    val read = (c: ConsoleReader) => Option(c.readLine("> "))
    val eval = (s: String) => i.eval(p.parse(s))
    val print = (d: Double) => println(d)

    try {
        val end = ((read andThen eval.lift andThen print.lift)(reader)).isEmpty
        if (!end) repl else ()
    } catch {
      case EvaluationException(m) =>
        println(m)
        repl
      case ParseException(m) =>
        println(m)
        repl
      case _: EOFException =>
    }
    println()
  }

  if (conf.interactive.isSupplied) repl
  else try {
    println(i.eval(p.parse(new Scanner(new File(conf.file())))))
  } catch {
      case EvaluationException(m) =>
        println(m)
      case ParseException(m) =>
        println(m)
  }

}

