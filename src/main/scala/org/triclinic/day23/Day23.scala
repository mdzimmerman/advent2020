package org.triclinic.day23

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Node(val n: Int) {
  var next: Node = this
}

class CircularList(init: Seq[Int]) {
  val map = mutable.HashMap[Int, Linked
  val list = mutable.LinkedList[Int]()
  var head: Option[Node] = None
  var length: Int = 0
  val map: mutable.Map[Int, Node] = mutable.HashMap[Int, Node]()

  def append(n: Int): Unit = {
    val node = new Node(n)
    map(n) = node
    head match {
      case Some(h) =>
        val l = h.prev
        l.next = node
        node.next = h
        length += 1
      case None =>
        head = Some(node)
        length = 1
    }
  }

  def insert

  def move() = {
    val p1 = head.get.next
    val p2 = p1.next
    val p3 = p2.next
    val p4 = p3.next

  }

  override def toString(): String = {
    head match {
      case Some(h) =>
        var curr = h
        val buffer = new ListBuffer[Int]()
        for (i <- 0 until length) {
          buffer.prepend(curr.n)
          curr = curr.next
        }
        s"CircularList(${buffer.reverse.mkString(" ")})"
      case None =>
        s"CircularList()"
    }
  }
}

object CircularList {
  def apply(s: String): CircularList = {
    val list = new CircularList()
    for (c <- s) {
      list.append(c.toString.toInt)
    }
    list
  }
}

case class State(cups: Vector[Int]) {
  def minus(n: Int): Int = if (n == 1) 9 else n-1

  def next(): State = {
    val curr = cups(0)
    val pickup = cups.slice(1, 4)
    val rest = cups.slice(4, cups.size)

    //println(s"curr=$curr pickup=$pickup rest=$rest")
    //println()

    var dest = minus(curr)
    while (pickup.contains(dest)) {
      dest = minus(dest)
    }
    val p = rest.indexOf(dest)
    val newcups = rest.slice(0, p+1) ++ pickup ++ rest.slice(p+1, rest.size) :+ curr
    //println(curr, pickup, rest)
    State(newcups)
  }

  def mkString: String = {
    val i = cups.indexOf(1)
    (cups.slice(i+1, cups.size) ++ cups.slice(0, i)).mkString("")
  }
}

object Day23 extends App {
  val test1 = State(Vector(3, 8, 9, 1, 2, 5, 4, 6, 7))

  val input = State(Vector(9, 6, 3, 2, 7, 5, 4, 8, 1))

  @tailrec
  private def applyN(curr: State, n: Int, i: Int): State = {
    //println(curr)
    if (i == n)
      curr
    else
      applyN(curr.next(), n, i+1)
  }

  def applyN(init: State, n: Int): State = applyN(init, n, 0)

  println(test1)
  println(applyN(test1, 10).mkString)
  println(applyN(test1, 100).mkString)

  println(applyN(input, 100).mkString)

  val c = CircularList("389125467")
  println(c)
  //c.append(1)
  //c.append(2)
  //c.append(3)
  //c.append(4)
  //c.append(5)
  //println(c)
}
