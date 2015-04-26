package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
//    var result: Map[String, Signal[Double]] = Map.empty

    for (
      (key, expr) <- namedExpressions
    ) yield key -> Signal[Double] {
      eval(expr(), namedExpressions)
    }
    /*    {
          result = result + (key -> Signal[Double]{
            eval(expr(), namedExpressions)
          })
        }
        result*/
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(l) =>
        l
      case Ref(refName) =>
        if(references.contains(refName))
          eval(references(refName)(), references - refName)
        else
          Double.NaN
      case Plus(a, b) =>
        eval(a, references) + eval(b, references)
      case Minus(a, b) =>
        eval(a, references) - eval(b, references)
      case Times(a, b) =>
        eval(a, references) * eval(b, references)
      case Divide(a, b) =>
        eval(a, references) / eval(b, references)

    }
  }

  /** Get the Expr for a referenced variables.
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
}
