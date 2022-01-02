package day04

import day04.Passport.{compulsoryFields, reColor, rePid, reCm, reIn}
import utils.InputLoader

case class Passport(fields: Map[String, String]) {
  def isValid1(): Boolean =
    compulsoryFields.forall(k => fields.keySet.contains(k))

  def isValid2(): Boolean =
    if (isValid1()) {
      isValidByr() &
        isValidIyr() &
        isValidEyr() &
        isValidHgt() &
        isValidHcl() &
        isValidEcl() &
        isValidPid()
    } else {
      false
    }

  def isValidByr(): Boolean = {
    val y = fields("byr").toInt
    y >= 1920 & y <= 2002
  }

  def isValidIyr(): Boolean = {
    val y = fields("iyr").toInt
    y >= 2010 & y <= 2020
  }

  def isValidEyr(): Boolean = {
    val y = fields("eyr").toInt
    y >= 2020 & y <= 2030
  }

  def isValidHgt(): Boolean = {
    fields("hgt") match {
      case reCm(y) => y.toInt >= 150 & y.toInt <= 193
      case reIn(y) => y.toInt >= 59 & y.toInt <= 76
      case _ => false
    }
  }

  def isValidHcl(): Boolean = {
    reColor.matches(fields("hcl"))
  }

  def isValidEcl(): Boolean = {
    Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(fields("ecl"))
  }

  def isValidPid(): Boolean = {
    rePid.matches(fields("pid"))
  }
}

object Passport {
  val reField = raw"(\w+):(.+)".r

  val compulsoryFields = List(
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid")

  val reCm = raw"(\d+)cm".r
  val reIn = raw"(\d+)in".r
  val reColor = raw"#[a-f0-9]{6}".r
  val rePid = raw"[0-9]{9}".r

  def parse(str: String): Passport =
    Passport(str.split("[\\s\n]+").filter(_.nonEmpty)
      .map { f =>
        f match {
          case reField(k, v) => k -> v
          case _ => throw new IllegalArgumentException()
        }
      }.toMap
    )

}

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
object PassportProcessing {
  def part1(passports: List[Passport]) = {

    val n = passports.filter {
      _.isValid1()
    }.length

    println(n)
  }

  def part2(passports: List[Passport]) = {

    val n = passports.filter {
      _.isValid2()
    }.length

    println(n)
  }


  def main(args: Array[String]) = {
    val input = InputLoader.read("04", false, "\n\n").map(Passport.parse)

    part1(input)
    part2(input)
  }
}
