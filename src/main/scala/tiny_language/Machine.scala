final class Machine {
  def run(statement: Statement, env: Map[String, Any]): Map[String, Any] = {
    println(s"\n\n*****\n-> $statement")
    try {
      runStatement(statement, env)
    } catch {
      case exc: Exception => env.updated("__error", exc.getMessage)
    }
  }

  private def runStatement(statement: Statement, env: Map[String, Any]): Map[String, Any] = statement match {
    case DoNothing() => env
    case Assign(name, expr) => assignStatement(name, reduce(expr, env), env)
    case IfElseStatement(expr, ifSt, elseSt) => ifElseStatement(reduce(expr, env), ifSt, elseSt, env)
    case Sequence(list) => list.foldLeft(env)((e, st) => runStatement(st, e))
    case WhileLoop(c, st) => whileLoopStatement(c, st, env)
  }

  private def assignStatement(name: String, expr: Expression, env: Map[String, Any]): Map[String, Any] = {
    env.updated(name, expr match {
      case Bool(b) => b
      case Number(n) => n
      case _ => throw new Exception(s"Not reducible $expr can't be assigned to $name")
    })
  }

  private def ifElseStatement(condition: Expression, ifSt: Statement, elseSt: Statement, env: Map[String, Any]): Map[String, Any] = {
    reduce(condition, env) match {
      case Bool(b) if b => runStatement(ifSt, env)
      case Bool(b) if !b => runStatement(elseSt, env)
      case _ => throw new Exception(s"Condition $condition should be bool")
    }
  }

  private def whileLoopStatement(condition: Expression, statement: Statement, env: Map[String, Any]): Map[String, Any] = {
    reduce(condition, env) match {
      case Bool(b) if b => runStatement(WhileLoop(condition, statement), runStatement(statement, env))
      case Bool(b) if !b => env
      case _ => throw new Exception(s"Condition $condition should be bool")
    }
  }

  def reduce(expr: Expression, env: Map[String, Any]): Expression = {
    if (expr.isReducible) {
      println(s"-> $expr")
      reduce(reductionStep(expr, env), env)
    }
    else {
      println(s"== $expr\n")
      expr
    }
  }

  def reductionStep(expr: Expression, env: Map[String, Any]): Expression = expr match {
    case Sum(lOp, rOp) => sumReduction(lOp, rOp, env)
    case Prod(lOp, rOp) => prodReduction(lOp, rOp, env)
    case Less(lOp, rOp) => lessReduction(lOp, rOp, env)
    case IfElse(condition, ifOp, elseOp) => ifElseReduction(condition, ifOp, elseOp, env)
    case Var(name) => varReduction(name, env)
  }

  private def sumReduction(lOp: Expression, rOp: Expression, env: Map[String, Any]): Expression = {
    binaryReduction(lOp, rOp, env=env,
      reduceStep=(o1, o2) => Sum(o1, o2),
      reduceFinal=(v1, v2) => Number(v1 + v2)
    )
  }

  private def prodReduction(lOp: Expression, rOp: Expression, env: Map[String, Any]) = {
    binaryReduction(lOp, rOp, env=env,
      reduceStep=(o1, o2) => Prod(o1, o2),
      reduceFinal=(v1, v2) => Number(v1 * v2)
    )
  }

  private def lessReduction(lOp: Expression, rOp: Expression, env: Map[String, Any]) = {
    binaryReduction(lOp, rOp, env=env,
      reduceStep=(o1, o2) => Less(o1, o2),
      reduceFinal=(v1, v2) => Bool(v1 < v2)
    )
  }

  private def binaryReduction(lOp: Expression, rOp: Expression, env: Map[String, Any],
                              reduceStep: (Expression, Expression) => Expression, reduceFinal: (Int, Int) => Expression): Expression = {
    (lOp, rOp) match {
      case (lOp: Number, rOp: Number) => reduceFinal(lOp.n, rOp.n)
      case (lOp: Expression, rOp: Expression) if lOp.isReducible => reduceStep(reductionStep(lOp, env), rOp)
      case (lOp: Expression, rOp: Expression) if rOp.isReducible => reduceStep(lOp, reductionStep(rOp, env))
      case _ => NonReducible(reduceStep(lOp, rOp))
    }
  }

  private def ifElseReduction(condition: Expression, ifOp: Expression, elseOp: Expression, env: Map[String, Any]) = {
    (condition, ifOp, elseOp) match {
      case (c: Bool, i: Expression, e: Expression) => if (c.v) i else e
      case (c: Expression, _, _) if c.isReducible => IfElse(reductionStep(c, env), ifOp, elseOp)
      case _ => NonReducible(IfElse(condition, ifOp, elseOp))
    }
  }

  private def varReduction(name: String, env: Map[String, Any]) = {
    env.getOrElse(name, Nil) match {
      case b: Boolean => Bool(b)
      case n: Int => Number(n)
      case _ => NonReducible(Var(name))
    }
  }
}
