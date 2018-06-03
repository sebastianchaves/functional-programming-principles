type Set = Int => Boolean

def contains(s: Set, elem: Int): Boolean = s(elem)

contains(Set(0), -1)

val set: Set = x => x > 0
val f: Int => Boolean = x => x + x == x
