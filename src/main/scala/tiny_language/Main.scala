object Main {
  def main(args: Array[String]) {
    val machine = new Machine
    val env = Map[String, Any]()
    val envx = Map("x" -> 9, "y" -> false)

    machine.reduce(Sum(Number(3), Sum(Number(1), Var("x"))), envx)

    machine.reduce(Prod(Sum(Number(10), Number(2)), Sum(Number(3), Number(4))), env)
    machine.reduce(Prod(Sum(Number(10), Number(2)), Number(1)), env)
    machine.reduce(Prod(Number(1), Sum(Number(10), Number(2))), env)

    machine.reduce(Less(Number(3), Number(5)), env)
    machine.reduce(Less(Number(7), Number(5)), env)

    machine.reduce(IfElse(Less(Number(5), Number(3)), Bool(true), Prod(Number(5), Sum(Prod(Number(3), Number(2)), Number(3)))), env)

    machine.reduce(Sum(Sum(Number(1), Sum(Number(2), Number(4))), Sum(Prod(Number(3), Number(2)), Number(3))), env)

    machine.reduce(Sum(Number(1), Bool(true)), env)
    machine.reduce(Prod(Sum(Number(9), Number(8)), Sum(Bool(true), Number(1))), env)
    machine.reduce(Sum(Number(3), Sum(Number(1), Var("z"))), envx)
    machine.reduce(Less(Number(7), Bool(true)), env)
    machine.reduce(Prod(IfElse(Number(5), Bool(true), Number(10)), Sum(Number(2), Prod(Number(11), Number(12)))), env)

    println(machine.run(Assign("x", Sum(Number(5), Var("x"))), envx))
    println(machine.run(Assign("x", Number(5)), envx))
    println(machine.run(Assign("z", Number(7)), envx))
    println(machine.run(Assign("x", Sum(Number(5), Var("d"))), envx))
    println(machine.run(DoNothing(), envx))

    println(machine.run(IfElseStatement(Less(Number(3), Number(5)), Assign("x", Number(99)), DoNothing()), envx))
    println(machine.run(IfElseStatement(Less(Number(7), Number(5)), Assign("x", Number(88)), DoNothing()), envx))

    println(machine.run(Sequence(
      List(Assign("a", Number(1)), Assign("b", Prod(Number(5), Number(6))), Assign("c", Number(42)))
    ), env))

    println(machine.run(WhileLoop(Less(Var("i"), Number(3)), Assign("i", Sum(Var("i"), Number(1)))), Map("i" -> 0)))
  }
}