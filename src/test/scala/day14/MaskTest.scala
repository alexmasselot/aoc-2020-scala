package day14

import org.specs2.mutable.Specification

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2022
 */
class MaskTest extends Specification {
  "Mask" should {
    val mask = Mask.parse("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
    "apply 11" in {
      val got = mask(11)

      got must equalTo(73)
    }
    "apply 101" in {
      val got = mask(101)

      got must equalTo(101)
    }
    "apply 0" in {
      val got = mask(0)

      got must equalTo(64)
    }
  }

  "FloatingMask" should {
    val mask = FloatingMask.parse("000000000000000000000000000000X1001X")
    "apply 42" in {
      val got = mask(42)
      got must equalTo(List(26, 27, 58, 59))
    }
    "fuzzyAdd" in {
      val got = mask.floatingAdd
      got must equalTo(List(0, 1, 32, 33))
    }
  }
  "companion" should {
    "parse" in {
      val given = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"

      val got = Mask.parse(given)

      got must equalTo(Mask(2 + 64, 64))
    }
  }
}
