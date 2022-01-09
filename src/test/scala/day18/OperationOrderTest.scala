package day18

import day18.OperationOrder.{compute, computeNoParenthesis}
import org.specs2.mutable.Specification

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2022
 */
class OperationOrderTest extends Specification {
  "computeNoParenthesis" should {
    "3 + 7" in {
      val got = computeNoParenthesis("3 + 7")
      got must equalTo(10)
    }
    "3 * 7" in {
      val got = computeNoParenthesis("3 * 7")
      got must equalTo(21)
    }
    "3 * 7 + 5" in {
      val got = computeNoParenthesis("3 * 7 + 5")
      got must equalTo(36)
    }
    "3 + 7 * 5" in {
      val got = computeNoParenthesis("3 + 7 * 5")
      got must equalTo(50)
    }
  }
  "compute" should {
    "1 + (2 * 3) + (4 * (5 + 6))" in {
      val got = compute("1 + (2 * 3) + (4 * (5 + 6))")
      got must equalTo(51)
    }
    "2 * 3 + (4 * 5)" in {
      val got = compute("2 * 3 + (4 * 5)")
      got must equalTo(46)
    }
    "5 + (8 * 3 + 9 + 3 * 4 * 3)" in {
      val got = compute("5 + (8 * 3 + 9 + 3 * 4 * 3)")
      got must equalTo(1445)
    }
    "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" in {
      val got = compute("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
      got must equalTo(669060)
    }
    "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" in {
      val got = compute("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
      got must equalTo(23340)
    }
  }
}
