package utils

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
case class Matrix[T](
                      val values: IndexedSeq[IndexedSeq[T]]
                    ) {
  if (values.map(r => r.length).distinct.size > 1) {
    throw new IllegalArgumentException("All rows do not have the same size")
  }
  val nRows = values.length
  val nCols = values(0).length

  def apply(row: Int, col: Int): T = values(row)(col)

  def appendRight(other: Matrix[T]): Matrix[T] = {
    Matrix(values.zip(other.values).map((rows) => rows._1 ++: rows._2))
  }

}

object Matrix {
  def build[T](values: Seq[Seq[T]]) =
    Matrix(values.map { r => r.toIndexedSeq }.toIndexedSeq)
}
