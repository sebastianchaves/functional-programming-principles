package week3

abstract class IntSet {
  def include(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(anotherSet: IntSet): IntSet
}

object Empty extends IntSet {
  def include(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def contains(x: Int): Boolean = false
  def union(anotherSet: IntSet): IntSet = anotherSet
  override def toString: String = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  def include(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left include x, right)
    else if (x > elem) new NonEmpty(elem, left, right include x)
    else this

  def contains(x: Int): Boolean =
    if (x > elem) right contains x
    else if (x < elem) left contains x
    else true

  def union(anotherSet: IntSet): IntSet = ((left union right) union anotherSet) include elem

  override def toString: String = s"{$left - $elem - $right}"

}