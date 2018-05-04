trait Statement

case class DoNothing() extends Statement {
  override def toString: String = "Nothing :)"
}

case class Assign(name: String, expression: Expression) extends Statement {
  override def toString: String = s"Assign: $name = $expression"
}

case class IfElseStatement(condition: Expression, ifStatement: Statement, elseStatement: Statement) extends Statement {
  override def toString: String = s"If: ($condition) $ifStatement else $elseStatement"
}

case class Sequence(list: List[Statement]) extends Statement {
  override def toString: String = "Sequence:\n" + list.map(s => s.toString).mkString("\n")
}

case class WhileLoop(condition: Expression, statement: Statement) extends Statement {
  override def toString: String = s"While $condition do $statement"
}