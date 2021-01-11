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
                     out: List[Int]): List[Int] = {
    if (curr == start)
      out.reverse
    else
      values(start, curr.next, curr.value :: out)
  }

  def values(start: Node): List[Int] = values(start, start.next, List(start.value))

  def values: List[Int] = values(head)

  override def toString: String =
    s"CircularList(${values.mkString(", ")})"
}

case class Cups(s: String) {
  val (list, map) = buildList(s)

  private def minus(n: Int): Int = if (n == 1) 9 else n-1

  private def buildList(s: String): (CircularList, Map[Int, Node]) = {
    val nodes = s.map(_.toString.toInt).map(Node.apply)
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
  val test1 = Cups("389125467")

  println(test1)
  test1.move(10)
  println(test1.state)
  test1.move(90)
  println(test1.state)

  val input = Cups("963275481")
  println(input)
  input.move(100)
  println(input.state)
}
