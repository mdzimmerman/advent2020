import scala.io.Source
import scala.util.matching.Regex

case class Password(low: Int,
                    high: Int,
                    char: Char,
                    password: String) {

  val count = password.groupBy(identity).mapValues(_.size)

  def isValid: Boolean = {
    val count = password.count(_ == char)
    count >= low && count <= high
  }

  def isValid2: Boolean = {
    val c1 = password(low-1) == char
    val c2 = password(high-1) == char
    c1 != c2
  }
}

object Password {
  val pattern: Regex = """(\d+)-(\d+) ([a-z]): (.+)""".r

  def apply(s: String): Option[Password] = {
    s match {
      case pattern(low, high, char, password) =>
        Some(Password(low.toInt, high.toInt, char(0), password))
      case _ =>
        None
    }
  }

  def parseLines(s: Seq[String]): List[Password] =
    s.flatMap(Password(_)).toList
}

val test1 = Password.parseLines(
  """
    |1-3 a: abcde
    |1-3 b: cdefg
    |2-9 c: ccccccccc
  """.stripMargin.split("""\n""").toList)

for (p <- test1) {
  println(f"$p => ${p.isValid}, ${p.isValid2}")
}
println(test1.count(_.isValid))
println(test1.count(_.isValid2))

val input = Password.parseLines(Source.fromFile("input.txt").getLines.toSeq)
println(input.count(_.isValid))
println(input.count(_.isValid2))