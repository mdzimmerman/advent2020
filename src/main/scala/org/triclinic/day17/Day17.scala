package org.triclinic.day17

import org.triclinic.Utils

import scala.annotation.tailrec

case class Cube(x: Int, y: Int, z: Int) {
  def neighbors(): IndexedSeq[Cube] =
    for (i <- x-1 to x+1;
         j <- y-1 to y+1;
         k <- z-1 to z+1 if !(i == x && j == y && k == z))
      yield Cube(i, j, k)
}

object Cube {
  def min(seq: Iterable[Cube]): Cube =
    Cube(
      seq.map(_.x).min,
      seq.map(_.y).min,
      seq.map(_.z).min)

  def max(seq: Iterable[Cube]): Cube =
    Cube(
      seq.map(_.x).max,
      seq.map(_.y).max,
      seq.map(_.z).max)

  def iter(min: Cube, max: Cube, buffer: Int=0): IndexedSeq[Cube] = {
    for (i <- min.x-buffer to max.x+buffer;
         j <- min.y-buffer to max.y+buffer;
         k <- min.z-buffer to max.z+buffer)
      yield Cube(i, j, k)
  }
}

case class State(active: Set[Cube]) {
  val min: Cube = Cube.min(active)
  val max: Cube = Cube.max(active)

  def countNeighbors(c: Cube): Int = {
    active.intersect(c.neighbors().toSet).size
  }

  def next(): State = {
    State(Cube.iter(min, max, 1).flatMap{ c => {
      val neighbors = countNeighbors(c)
      if (active.contains(c)) {
        if (neighbors == 2 || neighbors == 3) Some(c) else None
      } else {
        if (neighbors == 3) Some(c) else None
      }
    }}.toSet)
  }

  @tailrec
  private def run(n: Int, i: Int, curr: State): State = {
    //println(s"== step $i ==")
    //curr.printGrid()
    if (n == i)
      curr
    else
      run(n, i+1, curr.next())
  }

  def run(n: Int): State = run(n, 0, this)

  def printGrid(): Unit = {
    for (z <- min.z to max.z) {
      println(s"z=$z")
      for (y <- min.y to max.y) {
        for (x <- min.x to max.x) {
          if (active.contains(Cube(x, y, z)))
            print("#")
          else
            print(".")
        }
        println()
      }
      println()
    }
  }
}

object State {
  def apply(xs: Seq[String]): State = {
    val active = (for((row, j) <- xs.zipWithIndex; (c, i) <- row.zipWithIndex if c == '#') yield Cube(i, j, 0)).toSet
    State(active)
  }
}

case class HyperCube(x: Int, y: Int, z: Int, w: Int) {
  def neighbors(): IndexedSeq[HyperCube] =
    for (i <- x-1 to x+1;
         j <- y-1 to y+1;
         k <- z-1 to z+1;
         l <- w-1 to w+1 if !(i == x && j == y && k == z && w == l))
      yield HyperCube(i, j, k, l)
}

object HyperCube {
  def min(seq: Iterable[HyperCube]): HyperCube =
    HyperCube(
      seq.map(_.x).min,
      seq.map(_.y).min,
      seq.map(_.z).min,
      seq.map(_.w).min)

  def max(seq: Iterable[HyperCube]): HyperCube =
    HyperCube(
      seq.map(_.x).max,
      seq.map(_.y).max,
      seq.map(_.z).max,
      seq.map(_.w).max)

  def iter(min: HyperCube, max: HyperCube, buffer: Int=0): IndexedSeq[HyperCube] = {
    for (i <- min.x-buffer to max.x+buffer;
         j <- min.y-buffer to max.y+buffer;
         k <- min.z-buffer to max.z+buffer;
         l <- min.w-buffer to max.w+buffer)
      yield HyperCube(i, j, k, l)
  }
}

case class HyperState(active: Set[HyperCube]) {
  val min: HyperCube = HyperCube.min(active)
  val max: HyperCube = HyperCube.max(active)

  def countNeighbors(c: HyperCube): Int = {
    active.intersect(c.neighbors().toSet).size
  }

  def next(): HyperState = {
    HyperState(HyperCube.iter(min, max, 1).flatMap{ c => {
      val neighbors = countNeighbors(c)
      if (active.contains(c)) {
        if (neighbors == 2 || neighbors == 3) Some(c) else None
      } else {
        if (neighbors == 3) Some(c) else None
      }
    }}.toSet)
  }

  @tailrec
  private def run(n: Int, i: Int, curr: HyperState): HyperState = {
    //println(s"== step $i ==")
    //curr.printGrid()
    if (n == i)
      curr
    else
      run(n, i+1, curr.next())
  }

  def run(n: Int): HyperState = run(n, 0, this)

  def printGrid(): Unit = {
    for (w <- min.w to max.w; z <- min.z to max.z) {
      println(s"z=$z w=$w")
      for (y <- min.y to max.y) {
        for (x <- min.x to max.x) {
          if (active.contains(HyperCube(x, y, z, w)))
            print("#")
          else
            print(".")
        }
        println()
      }
      println()
    }
  }
}

object HyperState {
  def apply(xs: Seq[String]): HyperState = {
    val active = (for((row, j) <- xs.zipWithIndex; (c, i) <- row.zipWithIndex if c == '#') yield HyperCube(i, j, 0, 0)).toSet
    HyperState(active)
  }
}

object Day17 extends App {
  def testpart1(data: Seq[String]): Unit = {
    val test = State(data)
    val out = test.run(6)
    out.printGrid()
    println(out.active.size)
  }

  def testpart2(data: Seq[String]): Unit = {
    val test = HyperState(data)
    val out = test.run(6)
    out.printGrid()
    println(out.active.size)
  }

  def part1(data: Seq[String]): Unit = {
    val input = State(data)
    val out = input.run(6)
    //out.printGrid()
    println(out.active.size)
  }

  def part2(data: Seq[String]): Unit = {
    val input = HyperState(data)
    val out = input.run(6)
    //out.printGrid()
    println(out.active.size)
  }

  val test1 = Utils.readString(
    """
      |.#.
      |..#
      |###
      |""".stripMargin).filter(_.nonEmpty)

  testpart1(test1)
  testpart2(test1)

  val input = Utils.readString(
    """
      |##.#...#
      |#..##...
      |....#..#
      |....####
      |#.#....#
      |###.#.#.
      |.#.#.#..
      |.#.....#
      |""".stripMargin
  )

  part1(input)
  part2(input)
}
