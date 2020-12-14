package org.triclinic.day11

import org.triclinic.Utils

import scala.annotation.tailrec

case class Seats(grid: String, width: Int, height: Int) {
  def get(x: Int, y: Int): Char =
    if (x >= 0 && x < width && y >= 0 && y < height)
      grid.charAt(y * width + x)
    else
      '.'

  val dirs = List(
    (-1, -1),
    ( 0, -1),
    ( 1, -1),
    (-1,  0),
    ( 1,  0),
    (-1,  1),
    ( 0,  1),
    ( 1,  1))

  def neighbors(seats: Seats, x: Int, y: Int): Int = {
    val out = dirs.map{ case(dx, dy) => seats.get(x+dx, y+dy)}
    //println(out)
    out.count(_ == '#')
  }

  @tailrec
  final def firstSeat(x: Int, y: Int, dx: Int, dy: Int): Char = {
    if (x < 0 || x >= width || y < 0 || y >= height)
      '.'
    else get(x, y) match {
      case '#' => '#'
      case 'L' => 'L'
      case _ => firstSeat(x+dx, y+dy, dx, dy)
    }
  }

  def neighbors2(seats: Seats, x: Int, y: Int): Int =
    dirs.map{case(dx, dy) => seats.firstSeat(x+dx, y+dy, dx, dy)}.count(_ == '#')

  def showGrid(): String = {
    val s = new StringBuilder

    for (y <- 0 until height) {
      for (x <- 0 until width) {
        s += get(x, y)
      }
      s += '\n'
    }
    s.toString
  }

  def next(thres: Int, nf: (Seats, Int, Int) => Int): Seats = {
    val s = new StringBuilder
    for (y <- 0 until height; x <- 0 until width) {
      val c0 = get(x, y)
      val n = nf(this, x, y)
      val c1 = c0 match {
        case '.' => '.'
        case 'L' =>
          if (n == 0) '#' else 'L'
        case '#' =>
          if (n >= thres) 'L' else '#'
      }
      //println(f"$x, $y: ($n) $c0 -> $c1")
      s += c1
    }
    Seats(s.toString, width, height)
  }

  def stabilizeGen(thres: Int, nf: (Seats, Int, Int) => Int): Seats = {
    @tailrec
    def f(n: Int, seats0: Seats): Seats = {
      val seats1 = seats0.next(thres, nf)
      //println(seats1.showGrid())
      if (seats0 == seats1) {
        println(s"count = $n")
        seats1
      } else
        f(n+1, seats1)
    }
    f(0, this)
  }

  def stabilize(): Seats = stabilizeGen(4, neighbors)

  def stabilize2(): Seats = stabilizeGen(5, neighbors2)
}

object Seats {
  def apply(input: Seq[String]): Seats = {
    val length = input.length
    if (length == 0)
      throw new Exception("bad input")
    val width = input.head.length
    Seats(input.mkString(""), width, length)
  }
}


object Day11 extends App {
  def test1() {
    println("--- test 1 ---")
    val input = Seats(Utils.readResource("/day11/test1.txt"))
    println(input.showGrid())

    val stable = input.stabilize()
    println(stable.showGrid())
    println(stable.grid.count(_ == '#'))

    val stable2 = input.stabilize2()
    println(stable2.showGrid())
    println(stable2.grid.count(_ == '#'))
  }

  def input(): Unit = {
    println("--- input ---")
    val input = Seats(Utils.readResource("/day11/input.txt"))
    val stable = input.stabilize()
    println(stable.grid.count(_ == '#'))

    val stable2 = input.stabilize2()
    println(stable2.grid.count(_ == '#'))
  }

  test1()
  input()
  //println(test1.showGrid())
  //println(test1.next.showGrid())
}
