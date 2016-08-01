package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  private var dependencies = Set[String]()
  private var topname = ""

  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions map {
      case (name, exprSignal) => {
        topname = name
        dependencies = Set[String]()
        (name, Signal(eval(exprSignal(), namedExpressions)))
      }
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
    case Literal(v) => v
    case Ref(name) => {
      dependencies += name
      if (dependencies.contains(topname)) Double.NaN
      else eval(getReferenceExpr(name, references), references)
    }
    case Plus(a, b)   => eval(a, references) + eval(b, references)
    case Minus(a, b)  => eval(a, references) - eval(b, references)
    case Times(a, b)  => eval(a, references) * eval(b, references)
    case Divide(a, b) => eval(a, references) / eval(b, references)
  }

  /**
   * Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }

  def main(args: Array[String]) {
    val namedExprs = Map[String, Var[Expr]](
      "a" -> Var(Literal(1)),
      "b" -> Var(Plus(Ref("a"), Literal(1))))
      /*
      "c" -> Signal(Times(Ref("a"), Literal(3))),
      "d" -> Signal(Times(Ref("b"), Ref("c"))),
      "e" -> Signal(Times(Ref("f"), Literal(1))),
      "f" -> Signal(Times(Ref("e"), Literal(2))),
      "h" -> Signal(Times(Ref("a"), Ref("h"))),
      "g" -> Signal(Times(Ref("b"), Ref("h"))))
      */
    val results = computeValues(namedExprs)
    results foreach { case (name, sig) => println(name + "=" + sig()) }
    namedExprs("a")() = Plus(Ref("a"), Literal(1)) 
    results foreach { case (name, sig) => println(name + "=" + sig()) }
  }
}
