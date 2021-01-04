package org.triclinic.day20

import org.triclinic.{AsInt, Utils}

import scala.collection.immutable.Queue

case class Tile(id: Int,
                data: Array[Char],
                width: Int,
                height: Int) {
  lazy val sides = Vector(
    getRow(0),
    getCol(width - 1),
    getRow(height - 1).reverse,
    getCol(0).reverse)

  lazy val sidesRev = sides.map(_.reverse)

  lazy val sidesAll = sides ++ sidesRev

  def get(x: Int, y: Int): Char = data(y * width + x)

  def getRow(y: Int): String =
    (0 until width).map(x => data(y * width + x)).mkString("")

  def getCol(x: Int): String =
    (0 until height).map(y => data(y * width + x)).mkString("")

  def getSubTile(x1: Int, y1: Int, x2: Int, y2: Int): Tile = {
    val widthNew = x2 - x1 + 1
    val heightNew = y2 - y1 + 1
    val dataNew = (for (y <- y1 to y2; x <- x1 to x2) yield get(x, y)).toArray[Char]
    Tile(id, dataNew, widthNew, heightNew)
  }

  def getHashes() =
    (for (y <- 0 until height; x <- 0 until width if get(x, y) == '#')
      yield (x, y)).toList

  def rotateCW(): Tile = {
    val widthNew = height
    val heightNew = width
    val dataNew = Array.ofDim[Char](widthNew * heightNew)
    for (y <- 0 until height; x <- 0 until width)
      dataNew(y * widthNew + x) = get(y, width-x-1)
    Tile(id, dataNew, widthNew, heightNew)
  }

  def flip(): Tile = {
    val dataNew = Array.ofDim[Char](width * height)
    for (y <- 0 until height; x <- 0 until width)
      dataNew(y * width + x) = get(width-x-1, y)
    Tile(id, dataNew, width, height)
  }

  def joinHoriz(other: Tile): Tile = {
    if (height != other.height)
      throw new Exception("heights don't match")
    val widthNew = width + other.width
    val dataNew = Array.ofDim[Char](widthNew * height)
    for (y <- 0 until height) {
      for (x <- 0 until width)
        dataNew(y * widthNew + x) = get(x, y)
      for (x <- 0 until other.width)
        dataNew(y * widthNew + width + x) = other.get(x, y)
    }
    Tile(id, dataNew, widthNew, height)
  }

  def joinVert(other: Tile): Tile = {
    if (width != other.width)
      throw new Exception("widths don't match")
    val heightNew = height + other.height
    val dataNew = data ++ other.data
    Tile(id, dataNew, width, heightNew)
  }

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
      val data = xs.tail.mkString("").toCharArray
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

case class Side(tile1: Int, side1: Int, tile2: Int, side2: Int)

case class Pos(x: Int, y: Int)

case class TileList(tiles: Vector[Tile]) {
  val tileMap: Map[Int, Tile] = tiles.map(t => t.id -> t).toMap

  val connections: Map[Int, Vector[Side]] = {
    (for (ti <- tiles;
         tj <- tiles if ti != tj;
         si <- ti.sides.indices;
         sj <- tj.sidesAll.indices
          if ti.sidesAll(si) == tj.sidesAll(sj))
      yield Side(ti.id, si, tj.id, sj))
        .groupBy(x => x.tile1)
          .map{ case(k,v) => k -> v.sortBy(a => (a.tile1, a.side1)) }
  }

  val corners: List[Int] = connections.filter{ case(k, v) => v.size == 2 }.keys.toList

  def part1: Long = {
    println(corners)
    corners.map(_.toLong).product
  }

  def getCorner: Tile = {
    val cornerId = corners.head
    val corner = tileMap(cornerId)
    connections(cornerId).map(_.side1) match {
      case Vector(0, 1) => corner.rotateCW()
      case Vector(1, 2) => corner
      case Vector(2, 3) => corner.rotateCW().rotateCW().rotateCW()
      case Vector(3, 0) => corner.rotateCW().rotateCW()
    }
  }

  def search(currTile: Tile,
             currSide: Int,
             avail: Set[Int]): Option[Tile] = {
    def s(): Option[(Tile, Int)] = {
      for (a <- avail) {
        val atile = tileMap(a)
        for (s <- 4 to 7)
          if (currTile.sidesAll(currSide) == atile.sidesAll(s))
            return Option((atile, s))
        val rtile = atile.flip()
        for (s <- 4 to 7)
          if (currTile.sidesAll(currSide) == rtile.sidesAll(s))
            return Option((rtile, s))
      }
      None
    }
    s() match {
      case Some((tile, s)) =>
        currSide match {
          case 1 =>
            s match {
              case 4 => Option(tile.rotateCW().rotateCW().rotateCW())
              case 5 => Option(tile.rotateCW().rotateCW())
              case 6 => Option(tile.rotateCW())
              case 7 => Option(tile)
            }
          case 2 =>
            s match {
              case 4 => Option(tile)
              case 5 => Option(tile.rotateCW().rotateCW().rotateCW())
              case 6 => Option(tile.rotateCW().rotateCW())
              case 7 => Option(tile.rotateCW())
            }
        }
      case None =>
        None
    }
  }

  def aggregate(border: Queue[(Pos, Tile)],
               avail: Set[Int],
               map: Map[Pos, Tile]): Map[Pos, Tile] = {
    if (avail.isEmpty)
      map
    else {
      val ((posCurr, tileCurr), borderCurr) = border.dequeue
      val next = (1 to 2).map { d =>
        val posNext: Pos = d match {
          case 1 => Pos(posCurr.x+1, posCurr.y)
          case 2 => Pos(posCurr.x, posCurr.y+1)
        }
        search(tileCurr, d, avail) match {
          case Some(tileNext) => Some(posNext -> tileNext)
          case None => None
        }
      }.flatten
      val unavail = next.map(_._2.id).toSet
      aggregate(borderCurr ++ next, avail -- unavail, map ++ next)
    }
  }

  def aggregate(): Map[Pos, Tile] = {
    val pos = Pos(0, 0)
    val corner = getCorner
    val avail = tiles.map(_.id).toSet
    aggregate(Queue(pos -> corner), avail - corner.id, Map(pos -> corner))
  }

  def assemble(): Tile = {
    val agg = aggregate()
    val xmax = agg.keys.map(_.x).max
    val ymax = agg.keys.map(_.y).max
    val assemble = (0 to ymax).map(y =>
      (0 to xmax)
        .map(x => Pos(x, y))
        .map(agg(_).getSubTile(1, 1, 8, 8))
        .reduceLeft(_.joinHoriz(_))
    ).reduceLeft(_.joinVert(_))
    assemble
  }
}

object TileList {
  def parse(xs: List[String]): TileList = {
    TileList(Utils.split[String](xs, "").flatMap(Tile(_)).toVector)
  }
}

object Day20 extends App {
  val monster = Tile(Utils.readString(
    """Tile 1:
      |                  #.
      |#    ##    ##    ###
      | #  #  #  #  #  #...
      |""".stripMargin
  ).filter(_.nonEmpty).toList).get
  monster.printGrid()
  println(monster.getHashes())

  val test1 = TileList.parse(Utils.readResource("/day20/test1.txt").toList)
  val t0 = test1.tiles(0)
  println(s"Tile ${t0.id}:")
  t0.printGrid()
  println()
  //println("sides:")
  //for (s <- t0.sides)
  //  println(s"  $s")
  //println("sidesRev:")
  //for (s <- t0.sidesRev)
  //  println(s"  $s")
  //println("rotate CW:")
  //t0.rotateCW().printGrid()
  //println()
  //println("flip:")
  //t0.flip().printGrid()
  //println()
  //println("joinHoriz:")
  //t0.joinHoriz(t0.flip()).printGrid()
  //println()
  //println("joinVert:")
  //t0.joinVert(t0.rotateCW.rotateCW.flip).printGrid()
  //println("subtile(1,1,8,8):")
  //t0.getSubTile(1, 1, 8, 8).printGrid()

  println()
  println(test1.part1)

  for (c <- test1.connections)
    println(c)

  test1.assemble().printGrid()
  //for ((p, t) <- test1.assemble) {
  //  println(s"$p -> $t")
  //}


  //val input = TileList.parse(Utils.readResource("/day20/input.txt").toList)
  //println(input.part1)

  //for (t <- test1.tiles) {
  //  println(t.grid)
  //  println(t.getRow(0))
  //  println(t.getCol(t.width-1))
  //}
  //println(test1)
}