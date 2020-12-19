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

  @tailrec
  final def setv2(mask: List[Char], pos: Int, mem0: Long, out: List[Long]): List[Long] = {
    if (mask.isEmpty) {
      out
    } else {
      val outNew = mask.head match {
        case '0' =>
          getBit(mem0, pos) match {
            case 1L => out.map(setBit(_, pos))
            case 0L => out.map(unsetBit(_, pos))
          }
        case '1' => out.map(setBit(_, pos))
        case 'X' => out.map(unsetBit(_, pos)) ++ out.map(setBit(_, pos))
      }
      setv2(mask.tail, pos+1, mem0, outNew)
    }
  }

  def setv2(mem0: Long): List[Long] = setv2(mask.reverse.toList, 0, mem0, List(0L))

  def getBit(value: Long, pos: Int): Long = (value >> pos) & 1L

  def setBit(value: Long, pos: Int): Long = value | (1L << pos)

  def unsetBit(value: Long, pos: Int): Long = value & ~(1L << pos)
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

case class ProgramV2(instr: List[Instr]) {
  def run(instr: List[Instr],
          mask: Mask,
          memory: Map[Long, Long]): Map[Long, Long] = {
    //println(instr, mask, memory)
    if (instr.isEmpty) {
      memory
    } else {
      val (maskNew, memoryNew) = instr.head match {
        case m: Mask =>
          (m, memory)
        case a: Assign =>
          (mask, memory ++ mask.setv2(a.mem).map(i => i -> a.value))
      }
      run(instr.tail, maskNew, memoryNew)
    }
  }

  def run(): Map[Long, Long] = run(instr, Mask(""), Map())
}

object Day14 extends App {
  val test1 = Utils.readString(
    """
      |mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
      |mem[8] = 11
      |mem[7] = 101
      |mem[8] = 0
      |""".stripMargin).flatMap(Instr(_)).toList
  //println(test1)

  //val mask1 = test1.head.asInstanceOf[Mask]
  //println(mask1)
  //println(mask1.set(11))

  println("--- test part 1 ---")
  println(Program(test1).run().values.sum)

  //val mask2 = Mask("0011XXXX")
  //for (n <- mask2.setv2(0))
  //  println(n)
  //for (i <- 0 to 5) {
  //  println(mask2.getBit(26, i))
  //}

  //val mask3 = Mask("000000000000000000000000000000X1001X")
  //val mask3 = Mask("00X1001X")
  //for (n <- mask3.setv2(42))
  //  println(n)

  println("--- test part 2 ---")
  val test2 = Utils.readString(
    """
      |mask = 000000000000000000000000000000X1001X
      |mem[42] = 100
      |mask = 00000000000000000000000000000000X0XX
      |mem[26] = 1
    """.stripMargin).flatMap(Instr(_)).toList
  println(ProgramV2(test2).run().values.sum)

  println("--- input ---")
  val input = Utils.readResource("/day14/input.txt").flatMap(Instr(_)).toList
  //for ((k, v) <- Program(input).run()) {
  //  println(s"$k -> $v")
  //}
  println(Program(input).run().values.sum)
  println(ProgramV2(input).run().values.sum)


}
