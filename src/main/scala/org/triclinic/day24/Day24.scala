package org.triclinic.day24

import org.triclinic.Utils

import scala.annotation.tailrec
import scala.collection.mutable

case class Hex(q: Int, r: Int) {
  def neighbors(): Set[Hex] = Hex.dirs.map(this.move).toSet

  def move(dir: String): Hex = {
    dir match {
      case "e"  => Hex(q+1, r)
      case "se" => Hex(q,   r+1)
      case "sw" => Hex(q-1, r+1)
      case "w"  => Hex(q-1, r)
      case "nw" => Hex(q,   r-1)
      case "ne" => Hex(q+1, r-1)
    }
  }

  @tailrec
  final def moves(s: String, curr: Hex): Hex = {
      if (s.isEmpty)
        curr
      else {
        val (dir, rest) = s match {
          case Hex.pattern(d, r) => (d, r)
        }
        moves(rest, curr.move(dir))
      }
  }

  def moves(s: String): Hex = moves(s, this)
}

object Hex {
  val pattern = """^(e|se|sw|w|ne|nw)(.*)$""".r

  val dirs = List("e", "se", "sw", "w", "ne", "nw")

  def countBlack(xs: List[String]): Int = {
    val map = mutable.Map[Hex, Int]()
    for (x <- xs) {
      val h = Hex(0,0).moves(x)
      if (map.contains(h))
        map(h) += 1
      else
        map(h) = 1
    }
    println(map)
    map.values.count(_ % 2 == 1)
  }
}

case class State(black: Set[Hex]) {
  val qmin: Int = black.map(_.q).min
  val qmax: Int = black.map(_.q).max
  val rmin: Int = black.map(_.r).min
  val rmax: Int = black.map(_.r).max

  def countBlack(): Int = black.size

  def iterRange(): IndexedSeq[Hex] =
    for (q <- qmin-1 to qmax+1; r <- rmin-1 to rmax+1)
      yield Hex(q, r)

  def next(): State = {
    val next = iterRange().flatMap { h =>
      val ns = black.intersect(h.neighbors()).size
      if (black.contains(h)) {
        if (ns == 0 || ns > 2) None else Some(h)
      } else {
        if (ns == 2) Some(h) else None
      }
    }.toSet
    State(next)
  }

  @tailrec
  private def nextn(n: Int, i: Int, curr: State): State = {
    //println(s"i=$i black=${curr.countBlack()}")
    if (n == i)
      curr
    else
      nextn(n, i+1, curr.next)
  }

  def nextn(n: Int): State = nextn(n, 0, this)
}

object State {
  def apply(xs: List[String]): State = {
    val map = mutable.Map[Hex, Int]()
    for (x <- xs) {
      val h = Hex(0,0).moves(x)
      if (map.contains(h))
        map(h) += 1
      else
        map(h) = 1
    }
    State(map.filter{case (_, i) => i % 2 == 1}.keys.toSet)
  }
}

object Day24 extends App {
  val test1 = State(Utils.readString(
    """
      |sesenwnenenewseeswwswswwnenewsewsw
      |neeenesenwnwwswnenewnwwsewnenwseswesw
      |seswneswswsenwwnwse
      |nwnwneseeswswnenewneswwnewseswneseene
      |swweswneswnenwsewnwneneseenw
      |eesenwseswswnenwswnwnwsewwnwsene
      |sewnenenenesenwsewnenwwwse
      |wenwwweseeeweswwwnwwe
      |wsweesenenewnwwnwsenewsenwwsesesenwne
      |neeswseenwwswnwswswnw
      |nenwswwsewswnenenewsenwsenwnesesenew
      |enewnwewneswsewnwswenweswnenwsenwsw
      |sweneswneswneneenwnewenewwneswswnese
      |swwesenesewenwneswnwwneseswwne
      |enesenwswwswneneswsenwnewswseenwsese
      |wnwnesenesenenwwnenwsewesewsesesew
      |nenewswnwewswnenesenwnesewesw
      |eneswnwswnwsenenwnwnwwseeswneewsenese
      |neswnwewnwnwseenwseesewsenwsweewe
      |wseweeenwnesenwwwswnew
      |""".stripMargin).filter(_.nonEmpty).toList)

  val input = State(
    Utils.readResource("/day24/input.txt").toList)

  println(test1)
  println(test1.countBlack())
  println(test1.nextn(100).countBlack())

  println(input.countBlack())
  println(input.nextn(100).countBlack())
  //println(input.countBlack())
}
