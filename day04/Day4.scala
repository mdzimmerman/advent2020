import scala.collection.mutable
import scala.io.Source

class Passport {
  val map = mutable.Map[String, String]()

  val req = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  def add(key: String, value: String): Unit = {
    map += (key -> value)
  }

  def isPresent: Boolean = {
    val keys = map.keys.toSet
    req.subsetOf(keys)
  }

  def isValid: Boolean = {
    req.forall(validField(_))
  }

  val hgtPattern = "^([0-9]+)(in|cm)$".r
  val eclSet = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

  def validYear(year: String): Boolean = year.matches("^[0-9]{4}$")

  def validField(name: String): Boolean = {
    (name, map.get(name)) match {
      case ("byr", Some(byr)) =>
        validYear(byr) && byr.toInt >= 1920 && byr.toInt <= 2002
      case ("iyr", Some(iyr)) =>
        validYear(iyr) && iyr.toInt >= 2010 && iyr.toInt <= 2020
      case ("eyr", Some(eyr)) =>
        validYear(eyr) && eyr.toInt >= 2020 && eyr.toInt <= 2030
      case ("hgt", Some(hgt)) =>
        hgt match {
          case hgtPattern(h, "in") =>
            h.toInt >= 59 && h.toInt <= 76
          case hgtPattern(h, "cm") =>
            h.toInt >= 150 && h.toInt <= 193
          case _ =>
            false
        }
      case ("hcl", Some(hcl)) =>
        hcl.matches("^#[0-9a-f]{6}$")
      case ("ecl", Some(ecl)) =>
        eclSet.contains(ecl)
      case ("pid", Some(pid)) =>
        pid.matches("^[0-9]{9}$")
      case _ =>
        false
    }
  }

  override def toString(): String = s"Passport($map)"
}

object Passport {
  def apply(): Passport = new Passport()

  def fromFile(lines: Seq[String]): List[Passport] = {
    val passports = mutable.ListBuffer[Passport]()
    passports.prepend(Passport())
    for (l <- lines) {
      if (l == "") {
        passports.prepend(Passport())
      } else {
        for (pair <- l.split(" ")) {
          val s = pair.split(":")
          passports.head.add(s(0), s(1))
        }
      }
    }
    passports.toList.reverse
  }
}

println("--- test #1 ---")
val test1 = Passport.fromFile(Source.fromFile("test1.txt").getLines.toSeq)
for (t <- test1) {
  println(s"$t -> ${t.isPresent}")
}
println(test1.count(_.isPresent))

println()
println("--- test #2 ---")
val test2 = Passport.fromFile(Source.fromFile("test2.txt").getLines.toSeq)
for (t <- test2) {
  println(s"$t")
  for (r <- t.req)
    println(s"  $r => ${t.validField(r)}")
  println(s"  ${t.isValid}")
}
println(test2.count(_.isValid))

println()
println("--- input ---")
val input = Passport.fromFile(Source.fromFile("input.txt").getLines.toSeq)
println(s"isPresent = ${input.count(_.isPresent)}")
println(s"isValid   = ${input.count(_.isValid)}")