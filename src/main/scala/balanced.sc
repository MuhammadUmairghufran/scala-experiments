@annotation.tailrec
def balanceStack(list: List[Char], stack: List[Char]): List[Char] = {
  if (list.isEmpty && stack.nonEmpty) {
    return stack
  }
  if (list.isEmpty || (list.head == ')' && stack.isEmpty)) {
    return list
  }
  val newStack =
    if (list.head == '(') '(' +: stack
    else if (list.head == ')' && stack.nonEmpty) stack.tail
    else stack

  balanceStack(list.tail, newStack)
}

def balance(sentence: String): Boolean = {
  val list: List[Char] = sentence.toList
  val finalList = balanceStack(list, List())
  if (finalList.nonEmpty)
    print("Wrong brackets: " + finalList + "\n")
  finalList.isEmpty
}

balance("a(щось тут ( та й тут ) є)")
balance("a(щось тут ( та й тут ) (капець) є)")
balance("a(щось (aaa) тут ( та й тут ) (капець) є)")
balance("a(щось ((aaa)) тут ( та й тут ) (капець) є)")
balance("(щось тут ( та й тут ) i може ( ще й тут) є)")
balance("(())()")

balance("(a(щось (aaa) тут ( та й тут ) (капець) є)")
balance("a(щось ((aaa) тут ( та й тут ) (капець) є)")
balance("a(щось ((aaa))) тут ( та й тут ) (капець) є)")
balance("a(щось ((aaa))) тут ( та й тут )) (капець) є)")
balance(":-)")
balance("())(")


