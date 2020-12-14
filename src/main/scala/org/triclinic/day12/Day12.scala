package org.triclinic.day12

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

object Day12 extends App {
  def test1() {
    println(Dir.right(Dir.North, 90))
    println(Dir.right(Dir.North, 180))
    println(Dir.right(Dir.North, 270))
    println(Dir.left(Dir.North, 90))
    println(Dir.left(Dir.North, 180))
    println(Dir.left(Dir.North, 270))
  }

  test1()
}