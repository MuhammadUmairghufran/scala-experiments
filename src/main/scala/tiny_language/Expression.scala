trait Expression {
  def isReducible: Boolean = this match {
    case Number(_) | Bool(_) | NonReducible(_) => false
    case _ => true
  }
}

case class NonReducible(expr: Expression) extends Expression {
  override def toString = s"[$expr]"
}

case class Number(n: Int) extends Expression {
  override def toString: String = s"$n"
}

case class Bool(v: Boolean) extends Expression {
  override def toString: String = s"$v"
}

case class Var(s: String) extends Expression {
  override def toString: String = s"$s"
}

case class Sum(leftOperand: Expression, rightOperand: Expression) extends Expression {
  override def toString: String = s"$leftOperand + $rightOperand"
}

case class Prod(leftOperand: Expression, rightOperand: Expression) extends Expression {
  override def toString: String = (leftOperand, rightOperand) match {
    case (leftOperand: Sum, rightOperand: Sum) => s"($leftOperand) * ($rightOperand)"
    case (leftOperand: Sum, _) => s"($leftOperand) * $rightOperand"
    case (_, rightOperand: Sum) => s"$leftOperand * ($rightOperand)"
    case _ => s"$leftOperand * $rightOperand"
  }
}

case class Less(leftOperand: Expression, rightOperand: Expression) extends Expression {
  override def toString: String = s"$leftOperand < $rightOperand"
}

case class IfElse(condition: Expression, ifOperand: Expression, elseOperand: Expression) extends Expression {
  override def toString: String = s"if ($condition) $ifOperand else $elseOperand"
}
