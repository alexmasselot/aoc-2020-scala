package day19

import org.specs2.mutable.Specification

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2022
 */
class CheckGraphTest extends Specification {
  val inputA =
    """
      |0: 1 2
      |1: "a"
      |2: 1 3 | 3 1
      |3: "b"""".stripMargin.trim.split("\n")
  val inputB =
    """
      |0: 4 1 5
      |1: 2 3 | 3 2
      |2: 4 4 | 5 5
      |3: 4 5 | 5 4
      |4: "a"
      |5: "b"""".stripMargin.trim.split("\n")


  "CheckGraphTest" should {
    "parse" in {
      val got = CheckGraph.parse(inputA)
      got must equalTo(
        CheckGraph(
          root = CheckNodePair(
            CheckNodeValue('a'),
            CheckNodeOr(
              CheckNodePair(
                CheckNodeValue('a'),
                CheckNodeValue('b'),
              ),
              CheckNodePair(
                CheckNodeValue('b'),
                CheckNodeValue('a'),
              )
            )
          )
        )
      )
    }

    "node isValid" should {
      val cnValueA = CheckNodeValue('a')
      val cnValueB = CheckNodeValue('b')

      val cnPairAB = CheckNodePair(cnValueA, cnValueB)
      val cnOrAB = CheckNodeOr(cnValueA, cnValueB)
      "CheckerNodeValue OK" in {
        cnValueA.isValid("a") must equalTo(true)
      }
      "CheckerNodeValue KO" in {
        cnValueA.isValid("b") must equalTo(false)
      }
      "CheckNodeOr OK a" in {
        cnOrAB.isValid("a") must equalTo(true)
      }
      "CheckNodeOr OK b" in {
        cnOrAB.isValid("a") must equalTo(true)
      }
      "CheckNodeOr OK ab" in {
        cnOrAB.isValid("ab") must equalTo(false)
      }
      "CheckNodePair OK a" in {
        cnPairAB.isValid("a") must equalTo(false)
      }
      "CheckNodePair OK ab" in {
        cnPairAB.isValid("ab") must equalTo(true)
      }
    }

    "graph is valid" should{
      val graph = CheckGraph.parse(inputB)
      "ababbb" in {
        graph(0).isValid("ababbb") must equalTo(true)
      }
      "bababa" in {
        graph(0).isValid("bababa") must equalTo(false)
      }
      "abbbab" in {
        graph(0).isValid("abbbab") must equalTo(true)
      }
      "aaabbb" in {
        graph(0).isValid("aaabbb") must equalTo(false)
      }
      "aaaabbb" in {
        graph(0).isValid("aaaabbb") must equalTo(false)
      }
    }
  }
}
