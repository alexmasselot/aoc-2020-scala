package day23

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2022
 */
case class Ring(values: List[Int]) {
  def head = values.head

  def size = values.size

  def indexOf(x:Int) = values.indexOf(x)

  def remove(n: Int, after: Int): (Ring, List[Int]) = {
    val i = indexOf(after)
    if (i < size - n) {
      (Ring(values.take(i + 1) ++ values.drop(i + 1 + n)), values.drop(i + 1).take(n))
    } else {
      (Ring(values.drop(n - (size - i - 1)).dropRight(size - i - 1)), values.drop(i + 1) ++ values.take(n - (size - i - 1)))
    }
  }

  def nextInsertValue(after: Int): Int =
    values.filter(_ < after).maxOption.getOrElse(values.max)

  def insert(xs: List[Int], after: Int) = {
    val i = indexOf(after)
    Ring(values.take(i + 1) ++ xs ++ values.drop(i + 1))
  }

  def nextValue(after: Int): Int = {
    val i = values.indexOf(after)
    if (i == size - 1)
      head
    else {
      values(i + 1)
    }
  }

  def play1(n: Int, at: Int) = {
    val (shortRing, chunk) = remove(3, at)
    shortRing.insert(chunk, shortRing.nextInsertValue(at))
  }

  override def toString: String = values.mkString(" ")
}
