// Examples:
// (щось тут ( та й тут ) i може ( ще й тут) є)
// ось ще однин ( тут ( та й тут ) i може) (тут)
//

//def cutMiddle(list: List[Char]): List[Char] = {
//
//}

def balanceIterate(list: List[Char]): List[Char] = {
  if (list.isEmpty)
    return list
  if (list.head != '(' && list.head != ')')
    return balanceIterate(list.tail)
  if (list.last != '(' && list.last != ')')
    return balanceIterate(list.dropRight(1))
  if (list.head == '(' && list.last == ')')
    return balanceIterate(list.tail.dropRight(1))
  list
}

def balance(sentence: String): Boolean = {
  val list: List[Char] = sentence.toList

  val finalList = balanceIterate(list)

  if (finalList.nonEmpty)
    print(finalList)

  finalList.isEmpty
}

balance("a(щось тут ( та й тут ) є)")
balance("a(щось тут ( та й тут ) (капець) є)")



// идея - сделать рекурсию, которая будет проходить
// по списку до первой открытой  ххх закрытой дужки и выризать ее
// и так пока не сможет найти что не вырезать, а потом посмотрим
// остались дужки или нет