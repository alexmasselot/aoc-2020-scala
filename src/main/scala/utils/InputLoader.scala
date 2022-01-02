package utils

import scala.io.Source

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object InputLoader {
  def read(day: String, isSample: Boolean, separator: String = "\n"): List[String] = {
    val tag = if (isSample) "-sample" else ""

    val filename = s"src/main/scala/day$day/input$tag.txt"

    if (separator == "\n")
      Source.fromFile(filename).getLines().toList
    else
      Source.fromFile(filename).getLines().mkString("\n").split(separator).toList
  }
}
