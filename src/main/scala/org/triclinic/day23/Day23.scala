package org.triclinic.day23

import scala.annotation.tailrec

case class Node(value: Int) {
  var prev: Node = this
  var next: Node = this
}

class CircularList(var head: Node) {
  def append(node: Node): Unit = {
    val prev0 = head.prev
    prev0.next = node
    node.prev = prev0
    node.next = head
    head.prev = node
  }

  def rotate: Unit = {
    head = head.next
  }

  def insert(pos: Node, list: CircularList): Unit = {
    val posNext = pos.next
    val listEnd = list.head.prev

    pos.next = list.head
    list.head.prev = pos
    listEnd.next = posNext
    posNext.prev = listEnd
  }

  def splice(start: Node, end: Node): CircularList = {
    val startPrev = start.prev
    val endNext   = end.next
    startPrev.next = endNext
    endNext.prev = startPrev
    start.prev = end
    end.next = start
    new CircularList(start)
  }

  @tailrec
  private def values(start: Node,
                     curr: Node,
                     max: Int,
                     out: List[Int]): List[Int] = {
    if (curr == start || max == 0)
      out.reverse
    else {
      val maxnew = if (max < 0) max else max-1
      values(start, curr.next, maxnew, curr.value :: out)
    }
  }

  def values(start: Node, max: Int): List[Int] = values(start, start.next, max-1, List(start.value))

  def values(start: Node): List[Int] = values(start, start.next, -1, List(start.value))

  def values: List[Int] = values(head)

  override def toString: String =
    s"CircularList(${values.mkString(", ")})"
}

case class Cups(s: String, size: Int) {
  val (list, map) = buildList()

  private def minus(n: Int): Int = if (n == 1) size else n-1

  private def buildList(): (CircularList, Map[Int, Node]) = {
    val nodes = (s.map(_.toString.toInt) ++ (10 to size)).map(Node.apply)
    val clist = new CircularList(nodes.head)
    for (n <- nodes.tail)
      clist.append(n)
    (clist, nodes.map(n => n.value -> n).toMap)
  }

  def move(n: Int): Unit = {
    for (_ <- 0 until n) move
  }

  def move: Unit = {
    val s = list.head.next
    val e = s.next.next
    val pickup = list.splice(s, e)
    val pickupSet = pickup.values.toSet

    var dest = minus(list.head.value)
    while (pickupSet.contains(dest)) {
      dest = minus(dest)
    }
    list.insert(map(dest), pickup)
    list.rotate
  }

  def state: String = list.values(map(1)).tail.mkString("")
}

object Day23 extends App {
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + ((t1 - t0).toFloat / 1000000000.0) + "s")
    result
  }

  def part1(in: String, label: String) = {
    println(s"--- part 1 ($label) ---")
    val c = Cups(in, 9)
    println(c)
    c.move(100)
    println(c.state)
  }

  def part2(in: String, label: String) = {
    println(s"--- part 2 ($label) ---")
    val c = time { Cups(in, 1000000) }
    println(c)
    time { c.move(10000000) }
    val out = c.list.values(c.map(1), 3).tail
    println(out)
    println(out.map(_.toLong).product)
  }

  val test  = "389125467"
  val input = "963275481"

  part1(test, "test")
  part1(input, "input")

  part2(test, "test")
  part2(input, "input")
}
