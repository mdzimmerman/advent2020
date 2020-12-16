package org.triclinic.day12

import org.triclinic.{AsInt, Utils}

import scala.annotation.tailrec

object Dir extends Enumeration {
  type Dir = Value
  val North, East, South, West = Value

  def right(d: Dir, deg: Int): Dir = {
    var d1 = d
    for (_ <- 1 to deg / 90)
      d1 = right(d1)
    d1
  }

  def right(d: Dir): Dir = d match {
    case North => East
    case East  => South
    case South => West
    case West  => North
  }

  def left(d: Dir, deg: Int): Dir = {
    var d1 = d
    for (_ <- 1 to deg/90)
      d1 = left(d1)
    d1
  }

  def left(d: Dir): Dir = d match {
    case North => West
    case West  => South
    case South => East
    case East  => North
  }
}

case class Instr(letter: Char, n: Int)

object Instr {
  val pattern = """^([NESWLRF])(\d+)""".r

  def apply(s: String): Option[Instr] = s match {
    case pattern(c, AsInt(n)) => Some(Instr(c.head, n))
    case _ => None
  }
}

trait Traveler {
  def next(instr: Instr): Traveler

  @tailrec
  private def travel(instructions: List[Instr], curr: Traveler): Traveler =
  if (instructions.isEmpty)
    curr
  else {
    //println(curr)
    travel(instructions.tail, curr.next(instructions.head))
  }

  def travel(instructions: List[Instr]): Traveler = travel(instructions, this)
}

case class Pos(x: Int, y: Int, facing: Dir.Dir) extends Traveler {
  def move(dir: Dir.Dir, n: Int): Pos =
    dir match {
      case Dir.North => Pos(x, y-n, facing)
      case Dir.East  => Pos(x+n, y, facing)
      case Dir.South => Pos(x, y+n, facing)
      case Dir.West  => Pos(x-n, y, facing)
    }

  def rotateRight(deg: Int): Pos = Pos(x, y, Dir.right(facing, deg))

  def rotateLeft(deg: Int): Pos = Pos(x, y, Dir.left(facing, deg))

  def next(instruction: Instr): Pos = {
    instruction match {
      case Instr('N', n)   => move(Dir.North, n)
      case Instr('E', n)   => move(Dir.East, n)
      case Instr('S', n)   => move(Dir.South, n)
      case Instr('W', n)   => move(Dir.West, n)
      case Instr('R', deg) => rotateRight(deg)
      case Instr('L', deg) => rotateLeft(deg)
      case Instr('F', n)   => move(facing, n)
      case _ =>
        throw new Exception(s"invalid instruction '$instruction'")
    }
  }

  def dist(other: Pos): Int =
    Math.abs(x - other.x) + Math.abs(y - other.y)
}

case class Pos2(x: Int, y: Int, wx: Int, wy: Int) extends Traveler {
  def rotateRight(pos: Pos2): Pos2 = Pos2(pos.x, pos.y, pos.wy, -pos.wx)

  def rotateLeft(pos: Pos2): Pos2 = Pos2(pos.x, pos.y, -pos.wy, pos.wx)

  @tailrec
  final def applyN(pos: Pos2, n: Int)(f: Pos2 => Pos2): Pos2 =
    if (n <= 0)
      pos
    else
      applyN(f(pos), n-1)(f)

  def next(instr: Instr): Pos2 = {
    instr match {
      case Instr('N', n) => Pos2(x, y, wx, wy+n)
      case Instr('E', n) => Pos2(x, y, wx+n, wy)
      case Instr('S', n) => Pos2(x, y, wx, wy-n)
      case Instr('W', n) => Pos2(x, y, wx-n, wy)
      case Instr('R', deg) => applyN(this, deg / 90)(rotateRight)
      case Instr('L', deg) => applyN(this, deg / 90)(rotateLeft)
      case Instr('F', n) => Pos2(x+wx*n, y+wy*n, wx, wy)
    }
  }

  def dist(other: Pos2): Int =
    Math.abs(x-other.x) + Math.abs(y-other.y)
}


object Day12 extends App {
  def test1() {
    println("--- test 1 ---")
    println(Dir.right(Dir.North, 90))
    println(Dir.right(Dir.North, 180))
    println(Dir.right(Dir.North, 270))
    println(Dir.left(Dir.North, 90))
    println(Dir.left(Dir.North, 180))
    println(Dir.left(Dir.North, 270))
  }

  def test2(): Unit = {
    println("--- test 2 ---")
    val instr = Utils.readString(
      """
        |F10
        |N3
        |F7
        |R90
        |F11
      """.stripMargin).flatMap(Instr(_)).toList
    println(instr)
    val start = Pos(0, 0, Dir.East)
    val end = start.travel(instr)
    println(end)
    println(start.dist(end.asInstanceOf[Pos]))

    val start2 = Pos2(0, 0, 10, 1)
    val end2 = start2.travel(instr)
    println(end2)
    println(start2.dist(end2.asInstanceOf[Pos2]))
  }

  def input(): Unit = {
    println("--- input ---")
    val input = Utils.readResource("/day12/input.txt").flatMap(Instr(_)).toList
    val start = Pos(0, 0, Dir.East)
    val end = start.travel(input)
    println(end)
    println(start.dist(end.asInstanceOf[Pos]))

    val start2 = Pos2(0, 0, 10, 1)
    val end2 = start2.travel(input)
    println(end2)
    println(start2.dist(end2.asInstanceOf[Pos2]))
  }
  test1()
  test2()
  input()
}