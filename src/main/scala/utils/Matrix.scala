package utils

import utils.Direction.{DOWN, Direction, LEFT, RIGHT, UP}
import utils.TransformationUnit.{ROTATION, TransformationUnit, VERTICAL_SYMMETRY}

import scala.+:

object Direction extends Enumeration {
  type Direction = Value
  val LEFT, RIGHT, UP, DOWN = Value
}

object TransformationUnit extends Enumeration {
  type TransformationUnit = Value
  val VERTICAL_SYMMETRY, ROTATION = Value
}

case class Transformation(units: List[TransformationUnit])

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
  val nCols = if (nRows == 0) 0 else values(0).length

  def row(i: Int) = if (i >= 0)
    values(i)
  else
    values(nRows + i)

  def col(i: Int) = if (i >= 0)
    values.map { row =>
      row(i)
    }
  else
    values.map { row =>
      row(nCols + i)
    }

  def set(i: Int, j: Int, x: T) = {
    val targetRow = values(i)
    val modifiedRow = targetRow.take(j) :+ x :++ targetRow.drop(j + 1)
    Matrix(
      values.take(i) :+ modifiedRow :++ values.drop(i + 1)
    )
  }

  def apply(row: Int, col: Int): T = values(row)(col)

  def append(on: Direction, other: Matrix[T]): Matrix[T] = {
    if (nCols == 0) {
      return other
    }
    on match {
      case RIGHT =>
        assertSameNRows(other)
        Matrix(values.zip(other.values).map((rows) => rows._1 ++: rows._2))
      case LEFT =>
        assertSameNRows(other)
        Matrix(values.zip(other.values).map((rows) => rows._2 ++: rows._1))
      case UP =>
        assertSameNCols(other)
        Matrix(other.values ++: values)
      case DOWN =>
        assertSameNCols(other)
        Matrix(values ++: other.values)
    }
  }

  def append(on: Direction, vector: IndexedSeq[T]): Matrix[T] = {
    if (nCols == 0) {
      return on match {
        case RIGHT => Matrix(vector.map { x => IndexedSeq(x) })
        case LEFT => Matrix(vector.map { x => IndexedSeq(x) })
        case UP => Matrix(IndexedSeq(vector))
        case DOWN => Matrix(IndexedSeq(vector))
      }
    }
    on match {
      case LEFT =>
        assert {
          vector.size == nRows
        }
        Matrix(values.zip(vector).map { case (row, x) => row.prepended(x) })
      case RIGHT =>
        assert {
          vector.size == nRows
        }
        Matrix(values.zip(vector).map { case (row, x) => row.appended(x) })
      case UP =>
        assert {
          vector.size == nCols
        }
        Matrix(values.prepended(vector))
      case DOWN =>
        assert {
          vector.size == nCols
        }
        Matrix(values.appended(vector))
    }
  }

  def map[S](op: (T) => S): Matrix[S] = Matrix(values.map {
    _.map(op)
  })

  def count(op: (T) => Boolean) = values.map {
    _.count(op)
  }.sum

  def foldRows[S](init: S)(op: (S, T) => S) =
    values.map { row => row.foldLeft(init)(op) }

  def foldColumns[S](init: S)(op: (S, T) => S) = {
    values.foldLeft(Seq.fill(nCols)(init)) { case (acc, row) =>
      acc.zip(row).map { case (x, y) => op(x, y) }
    }.toIndexedSeq
  }

  def transformUnit(transformationUnit: TransformationUnit) = transformationUnit match {
    case VERTICAL_SYMMETRY => Matrix(values.reverse)
    case ROTATION => {
      def handler(remain: IndexedSeq[IndexedSeq[T]]): IndexedSeq[IndexedSeq[T]] = {
        val newRow = IndexedSeq(remain.map {
          _.head
        })
        if (remain.head.size == 1) return newRow
        handler(remain.map {
          _.tail
        }) :++ newRow
      }

      Matrix(handler(values))
    }
  }

  def transform(units: Seq[TransformationUnit]) =
    units.foldLeft(this) { case (acc, u) => acc.transformUnit(u) }

  def scan[S](from: Direction, init: S)(op: (S, T) => S) = from match {
    case LEFT => Matrix(
      values.map {
        _.scanLeft(init) { case (x, y) => op(x, y) }.drop(1)
      }
    )
    case RIGHT => Matrix(
      values.map {
        _.scanRight(init) { case (y, x) => op(x, y) }.dropRight(1)
      }
    )
    case UP => Matrix(
      values.scanLeft(IndexedSeq.fill(nCols)(init)) { case (acc, row) => acc.zip(row).map { case (x, y) => op(x, y) } }.drop(1)
    )
    case DOWN => Matrix(
      values.scanRight(IndexedSeq.fill(nCols)(init)) { case (acc, row) => acc.zip(row).map { case (y, x) => op(x, y) } }.dropRight(1)
    )
  }

  def scanDiag[S](fromVert: Direction, fromHoriz: Direction, init: S)(op: (S, T) => S): Matrix[S] = {

    def handler(accRow: IndexedSeq[S], accCol: IndexedSeq[S], remain: Matrix[T]): Matrix[S] = {
      if (remain.nCols == 0 | remain.nRows == 0) {
        return Matrix.build(List())
      }
      val borderRow = fromHoriz match {
        case LEFT => accRow.zip(remain.take(fromVert)).map { case (acc, x) => op(acc, x) }
        case RIGHT => accRow.drop(1).zip(remain.take(fromVert)).map { case (acc, x) => op(acc, x) }
      }
      val remain2 = remain.drop(fromVert, 1)
      val borderCol = fromVert match {
        case UP => accCol.zip(remain2.take(fromHoriz)).map { case (acc, x) => op(acc, x) }
        case DOWN => accCol.drop(1).zip(remain2.take(fromHoriz)).map { case (acc, x) => op(acc, x) }
      }
      handler(borderRow, borderCol, remain2.drop(fromHoriz, 1)).append(fromHoriz, borderCol).append(fromVert, borderRow)
    }

    handler(IndexedSeq.fill(nCols + 1)(init), IndexedSeq.fill(nRows)(init), this)
  }

  def shift(from: Direction, defaultValue: T): Matrix[T] = from match {
    case RIGHT => Matrix(values.map {
      _.drop(1) :+ defaultValue
    })
    case LEFT => Matrix(values.map {
      defaultValue +: _.dropRight(1)
    })
    case UP => Matrix(IndexedSeq.fill(nCols)(defaultValue) +: values.dropRight(1))
    case DOWN => Matrix(values.drop(1) :+ IndexedSeq.fill(nCols)(defaultValue))
  }

  def zip[S](other: Matrix[S]): Matrix[(T, S)] = {
    assertSameDimensions(other)
    Matrix(
      values.zip(other.values).map { case (row, otherRow) => row.zip(otherRow) }
    )
  }

  def zipN(others: Matrix[T]*): Matrix[List[T]] = {
    others.foreach(assertSameDimensions)
    others.foldLeft(this.map { x => List(x) }) { case (acc, o) => acc.zip(o).map { case (l, x) => l :+ x } }
  }

  def drop(from: Direction, n: Int) = from match {
    case LEFT => Matrix(values.map {
      _.drop(n)
    })
    case RIGHT => Matrix(values.map {
      _.dropRight(n)
    })
    case UP => Matrix(values.drop(n))
    case DOWN => Matrix(values.dropRight(n))
  }
  def shrink()= Matrix(
    values.drop(1).dropRight(1).map{row => row.drop(1).dropRight(1)}
  )

  def take(from: Direction) = from match {
    case LEFT => values.map {
      _.head
    }
    case RIGHT => values.map {
      _.last
    }
    case UP => values.head
    case DOWN => values.last
  }

  def range(rows: Range, cols: Range) = Matrix(
    values.drop(rows.start).take(rows.end - rows.start).map {
      _.drop(cols.start).take(cols.end - cols.start)
    }
  )

  /**
   * build a list of surrounding
   *
   * @return
   */
  def neighbors8(): Matrix[List[T]] = {
    val matOpt: Matrix[Option[T]] = map {
      Some(_)
    }
    val lr = matOpt.shift(LEFT, None).zipN(matOpt, matOpt.shift(RIGHT, None))

    matOpt.shift(LEFT, None).zipN(matOpt.shift(RIGHT, None))
      .zip(lr.shift(UP, List()))
      .zip(lr.shift(DOWN, List()))
      .map { case ((xs, us), ds) => xs ::: us ::: ds }
      .map { xs =>
        xs.filter {
          _.isDefined
        }
      }
      .map { xs =>
        xs.map {
          _.get
        }
      }
  }

  def assertSameNRows[S](other: Matrix[S]) = {
    if (nRows != other.nRows) {
      throw new UnsupportedOperationException(s"Not the same number of rows ($nRows x $nCols) vs (${other.nRows} x ${other.nCols})")
    }
  }

  def assertSameNCols[S](other: Matrix[S]) = {
    if (nCols != other.nCols) {
      throw new UnsupportedOperationException(s"Not the same number of columns ($nRows x $nCols) vs (${other.nRows} x ${other.nCols})")
    }
  }

  def assertSameDimensions[S](other: Matrix[S]) = {
    assertSameNCols(other)
    assertSameNRows(other)
  }

  def toString(sep: String): String = values.map {
    _.mkString(sep)
  }.mkString("\n")

  override def toString: String = toString(" ")
}

object Matrix {
  def build[T](values: Seq[Seq[T]]) =
    Matrix(values.map { r => r.toIndexedSeq }.toIndexedSeq)

  def buildChar(input: List[String]): Matrix[Char] =
    Matrix(input.map {
      _.split("").filter(!_.isEmpty).map {
        _ (0)
      }.toIndexedSeq
    }.toIndexedSeq)

  def fillRows[T](nRows: Int, row: Seq[T]): Matrix[T] = build(Seq.fill(nRows)(row))

  def fillColumns[T](nCols: Int, column: Seq[T]): Matrix[T] = build(column.map { x => Seq.fill(nCols)(x) })

  def fill[T](nRows: Int, nCols: Int)(init: T) = Matrix(IndexedSeq.fill(nRows)(IndexedSeq.fill(nCols)(init)))
}
