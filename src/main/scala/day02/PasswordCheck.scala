package day02

/**
 * @author Alexandre Masselot
 * @Copyright L'Occitane 2021
 */
case class PasswordCheck(
                          val range: Range,
                          val char: Char,
                          val password: String
                        ) {
  def isValid1() = {
    range.contains(password.count(c => c == char))
  }

  def isValid2() = {
    (password(range.start-1)==char & password(range.end-1)!=char) |(password(range.start-1)!=char & password(range.end-1)==char)
  }
}
