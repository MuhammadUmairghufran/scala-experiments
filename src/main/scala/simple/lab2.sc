type Set = Int => Boolean

def nSet(): Set = x => x > 0
nSet()(-1)
nSet()(1)

def contains(s: Set, elem: Int): Boolean = s(elem)
assert(contains(Set(1), 1))

def singletonSet(elem: Int): Set = a => a == elem
assert(singletonSet(1)(1))
assert(!singletonSet(1)(2))

def union(s: Set, t: Set): Set = a => s(a) || t(a)
assert(union(Set(1), Set(2))(1))
assert(!union(Set(1), Set(2))(3))

def intersect(s: Set, t: Set): Set = a => s(a) && t(a)
assert(intersect(Set(1), Set(1))(1))
assert(!intersect(Set(1), Set(2))(1))

def diff(s: Set, t: Set): Set = a => s(a) && !t(a)
assert(diff(Set(1), Set(0))(1))
assert(!diff(Set(1), Set(1))(1))
assert(diff(Set(1), Set(0))(1))

def filter(s: Set, p: Int => Boolean): Set = a => s(a) && p(a)
assert(filter(Set(1, 2, 3, 4), x => x > 2)(3))
assert(!filter(Set(1, 2, 3, 4), x => x > 2)(1))
assert(!filter(Set(1, 2, 3, 4), x => x > 2)(5))

def forall(s: Set, p: Int => Boolean): Boolean = {
  @annotation.tailrec
  def iter(a: Int): Boolean = {
    if (s(a) && !p(a)) false
    else if (a == 1000) true
    else iter(a + 1)
  }

  iter(-1000)
}
assert(forall(Set(1, 2, 3), x => x <= 3))
assert(!forall(Set(1, 2, 3, 1000), x => x <= 3))
assert(forall(Set(1, 2, 3, 1001), x => x <= 3))
assert(forall(Set(1, 2, 3, -1001), x => x >= 0 && x <= 3))

def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, n => !p(n))
assert(!exists(Set(1, 2, 3), x => x == 0))
assert(exists(Set(1, 2, 3), x => x == 1))
assert(exists(Set(1, 2, 3), x => x == 3))
assert(!exists(Set(1, 2, 3), x => x == 4))

// so it's like inverse process -
// we apply function not for set, but for element that we apply on set
// don't know if this is what is needed
// don't understand how to apply function directly for set elements
// maybe with some forall analogue that works with Int => Int
def map(s: Set, f: Int => Int): Set = (a: Int) => s(f(a))
map(Set(4), x => x * 2)(2)





