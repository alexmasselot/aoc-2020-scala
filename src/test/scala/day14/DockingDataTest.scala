package day14

import org.specs2.mutable.Specification

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2022
 */
class DockingDataTest extends Specification {

  "DockingDataTest" should {
    "processFuzzy two instructions" in {
      val instructions = """mask = 000000000000000000000000000000X1001X
                           |mem[42] = 100
                           |mask = 00000000000000000000000000000000X0XX
                           |mem[26] = 1""".stripMargin.split("\n").toList

      val got = DockingData.processFloating(instructions)
      println(s" register floating: $got")

      got.regs.keys.toSet.toList.sorted must equalTo(Set(26,27,58,59,16,17,18,19,24,25,26,27).toList.sorted)
      got.sum must equalTo(208)
    }
  }
}
