def balance(chars: List[Char]): Boolean = {

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

  val finalList = balanceStack(chars, List())
  if (finalList.nonEmpty)
    print("Wrong brackets: " + finalList + "\n")
  finalList.isEmpty
}

def balanceBrackets(sentence: String): Boolean = {
  balance(sentence.toList)
}

balanceBrackets("a(щось тут ( та й тут ) є)")
balanceBrackets("a(щось тут ( та й тут ) (капець) є)")
balanceBrackets("a(щось (aaa) тут ( та й тут ) (капець) є)")
balanceBrackets("a(щось ((aaa)) тут ( та й тут ) (капець) є)")
balanceBrackets("(щось тут ( та й тут ) i може ( ще й тут) є)")
balanceBrackets("(())()")

balanceBrackets("(a(щось (aaa) тут ( та й тут ) (капець) є)")
balanceBrackets("a(щось ((aaa) тут ( та й тут ) (капець) є)")
balanceBrackets("a(щось ((aaa))) тут ( та й тут ) (капець) є)")
balanceBrackets("a(щось ((aaa))) тут ( та й тут )) (капець) є)")
balanceBrackets(":-)")
balanceBrackets("())(")


