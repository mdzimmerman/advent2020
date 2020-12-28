package org.triclinic.day20

import org.triclinic.{AsInt, Utils}

case class Tile(id: Int,
                data: Vector[Char],
                width: Int,
                height: Int) {
  val sides = Vector(
    getRow(0),
    getCol(width-1),
    getRow(height-1).reverse,
    getCol(0).reverse)

  val sidesRev = sides.map(_.reverse)

  val sidesAll = sides ++ sidesRev

  def get(x: Int, y: Int) = data(y * width + x)

  def getRow(y: Int): String =
    (0 until width).map(x => data(y * width + x)).mkString("")

  def getCol(x: Int): String =
    (0 until height).map(y => data(y * width + x)).mkString("")

  def printGrid(): Unit = {
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        print(get(x, y))
      }
      println()
    }
  }
}

object Tile {
  private val pattern = """Tile (\d+):""".r

  def apply(xs: List[String]): Option[Tile] = {
    if (xs.length < 2) {
      None
    } else {
      val data = xs.tail.mkString("").toVector
      val width = xs.tail.map(_.length).max
      val height = xs.tail.size
      xs.head match {
        case pattern(AsInt(id)) =>
          Some(Tile(id, data, width, height))
        case _ =>
          None
      }
    }
  }
}

case class Side(tile: Int, side: Int)

case class TileList(tiles: Vector[Tile]) {
  def connections() = {
    for (ti <- tiles;
         tj <- tiles if ti != tj;
         si <- ti.sides.indices;
         sj <- tj.sidesAll.indices
          if ti.sidesAll(si) == tj.sidesAll(sj))
      yield (Side(ti.id, si), Side(tj.id, sj))
  }

  def part1: Long= {
    val corners = connections().groupBy(_._1.tile).filter {
      case (k, v) => v.size == 2
    }.keys.toList.map(_.toLong)
    println(corners)
    corners.product
  }
}

object TileList {
  def parse(xs: List[String]): TileList = {
    TileList(Utils.split[String](xs, "").flatMap(Tile(_)).toVector)
  }
}

object Day20 extends App {
  val test1 = TileList.parse(Utils.readResource("/day20/test1.txt").toList)
  val t0 = test1.tiles(0)
  println(s"Tile ${t0.id}:")
  t0.printGrid()
  println()
  println("sides:")
  for (s <- t0.sides)
    println(s"  $s")
  println("sidesRev:")
  for (s <- t0.sidesRev)
    println(s"  $s")

  println(test1.part1)

  val input = TileList.parse(Utils.readResource("/day20/input.txt").toList)
  println(input.part1)

  //for (t <- test1.tiles) {
  //  println(t.grid)
  //  println(t.getRow(0))
  //  println(t.getCol(t.width-1))
  //}
  //println(test1)
}