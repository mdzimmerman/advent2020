package org.triclinic.day14

import org.triclinic.{AsInt, AsLong, Utils}

import scala.annotation.tailrec

trait Instr

case class Mask(mask: String) extends Instr {
  val maskBits = mask.reverse.zipWithIndex.flatMap{
    case('0', i) => Some(i -> 0)
    case('1', i) => Some(i -> 1)
    case _ => None
  }.toMap

  @tailrec
  final def set(mask: Map[Int, Int], value: Long): Long =
    if (mask.isEmpty)
      value
    else {
      val (pos, bit) = mask.head
      val valueNew = bit match {
        case 1 => value | (1L << pos)
        case 0 => value & ~(1L << pos)
      }
      set(mask.tail, valueNew)
    }

  def set(value: Long): Long = set(maskBits, value)
}

case class Assign(mem: Int, value: Long) extends Instr

object Instr {
  val maskPat   = """mask = ([X01]+)""".r
  val assignPat = """mem\[(\d+)\] = (\d+)""".r

  def apply(s: String): Option[Instr] = s match {
    case maskPat(mask) =>
      Some(Mask(mask))
    case assignPat(AsInt(mem), AsLong(value)) =>
      Some(Assign(mem, value))
    case _ => None
  }
}

case class Program(instr: List[Instr]) {
  def run(instr: List[Instr],
          mask: Mask,
          memory: Map[Int, Long]): Map[Int, Long] = {
    //println(instr, mask, memory)
    if (instr.isEmpty) {
      memory
    } else {
      val (maskNew, memoryNew) = instr.head match {
        case m: Mask =>
          (m, memory)
        case a: Assign =>
          (mask, memory + (a.mem -> mask.set(a.value)))
      }
      run(instr.tail, maskNew, memoryNew)
    }
  }

  def run(): Map[Int, Long] = run(instr, Mask(""), Map())
}

object Program {
  def apply(xs: Seq[String]): Program =
    Program(xs.filter(_.nonEmpty).flatMap(Instr(_)).toList)
}

object Day14 extends App {
  val test1 = Program(Utils.readString(
    """
      |mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
      |mem[8] = 11
      |mem[7] = 101
      |mem[8] = 0
      |""".stripMargin))
  println(test1)
  val mask1 = test1.instr.head.asInstanceOf[Mask]
  //println(mask1)
  //println(mask1.set(11))
  println(test1.run().values.sum)

  val input = Program(Utils.readResource("/day14/input.txt"))
  for ((k, v) <- input.run()) {
    println(s"$k -> $v")
  }
  println(input.run().values.sum)
}
