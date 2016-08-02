package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  private var dependencies = Map[String, Set[String]]()

  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    dependencies = namedExpressions map {
      case (name, _) => (name, Set[String]())
    }
    namedExpressions map {
      case (name, exprSignal) => {
        (name, Signal(eval(exprSignal(), namedExpressions, name)))
      }
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]], exprName: String): Double = expr match {
    case Literal(v) => v
    case Ref(name) => {
      if (dependencies.contains(name)) {
        dependencies +=  (exprName -> (dependencies(exprName) + name))
        dependencies +=  (exprName -> (dependencies(exprName) ++ dependencies(name)))
        if (dependencies(exprName) contains exprName) Double.NaN
        else eval(getReferenceExpr(name, references), references, name) 
      } else Double.NaN
      
    }
    case Plus(a, b)   => eval(a, references, exprName) + eval(b, references, exprName)
    case Minus(a, b)  => eval(a, references, exprName) - eval(b, references, exprName)
    case Times(a, b)  => eval(a, references, exprName) * eval(b, references, exprName)
    case Divide(a, b) => eval(a, references, exprName) / eval(b, references, exprName)
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
      "b" -> Var(Plus(Ref("a"), Literal(1))),
      "c" -> Var(Times(Ref("a"), Literal(3))),
      "d" -> Var(Times(Ref("b"), Ref("c"))),
      "e" -> Var(Times(Ref("f"), Literal(1))),
      "f" -> Var(Times(Ref("e"), Literal(2))),
      "h" -> Var(Times(Ref("a"), Ref("h"))),
      "g" -> Var(Times(Ref("b"), Ref("h")))
    )
    val results = computeValues(namedExprs)
    results foreach { case (name, sig) => println(name + "=" + sig()) }
    //namedExprs("a")() = Plus(Ref("a"), Literal(1)) 
    //results foreach { case (name, sig) => println(name + "=" + sig()) }
  }
}
