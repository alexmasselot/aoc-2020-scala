package day07

import org.specs2.mutable.Specification
import utils.InputLoader

/**
 *   @author Alexandre Masselot
 *   @Copyright L'Occitane 2022
 */
class BagGraphParserTest extends Specification {
  "parse Sample 2" >> {
    val input = InputLoader.read("07", true)

    val graph = BagGraphParser.parse(input)

    graph.edges must haveSize(13)
    graph.nodes must haveSize(9)

  }
}
