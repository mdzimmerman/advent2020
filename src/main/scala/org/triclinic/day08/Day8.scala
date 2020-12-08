package org.triclinic.day08

import org.triclinic.Utils

import scala.collection.mutable

sealed trait Operation
case object Acc extends Operation
case object Jmp extends Operation
case object Nop extends Operation

object Operation {
  def apply(s: String): Option[Operation] = s match {
    case "acc" => Some(Acc)
    case "jmp" => Some(Jmp)
    case "nop" => Some(Nop)
    case _ => None
  }
}

case class Instruction(operation: Operation, argument: Int)

object Instruction {
  val pattern = """(...) ([-+]\d+)""".r

  def apply(s: String): Option[Instruction] = s match {
    case pattern(opStr, argStr) =>
      Operation(opStr) match {
        case Some(op) => Some(Instruction(op, argStr.toInt))
        case None => None
      }
    case _ =>
      None
  }
}

object State extends Enumeration {
  type State = Value
  val Running, Terminated, InfLoop, Error = Value
}

case class Output(state: State.State, accumulator: Int)

case class Program(instructions: Vector[Instruction]) {
  def genVariants(): Iterator[Program] =
    instructions.indices.toIterator.flatMap{ i =>
      instructions(i) match {
        case Instruction(Nop, arg) =>
          Some(Program(instructions.updated(i, Instruction(Jmp, arg))))
        case Instruction(Jmp, arg) =>
          Some(Program(instructions.updated(i, Instruction(Nop, arg))))
        case _ =>
          None
      }
    }

  def run(): Output = {
    val seen = mutable.Set[Int]()
    var accumulator: Int = 0
    var pointer: Int = 0
    while (!seen.contains(pointer) && pointer >= 0 && pointer < instructions.size) {
      seen += pointer
      //println(s"$pointer => ${instructions(pointer)}")
      instructions(pointer) match {
        case Instruction(Acc, arg) =>
          accumulator += arg
          pointer += 1
        case Instruction(Jmp, arg) =>
          pointer += arg
        case Instruction(Nop, arg) =>
          pointer += 1
      }
    }
    if (seen.contains(pointer))
      Output(State.InfLoop, accumulator)
    else if (pointer == instructions.size)
      Output(State.Terminated, accumulator)
    else
      Output(State.Error, accumulator)

  }
}

object Program {
  def parse(xs: Seq[String]): Program =
    Program(xs.flatMap(Instruction(_)).toVector)
}

object Day8 extends App {
  println("--- test1 ---")
  val test1 = Program.parse(Utils.readString(
    """
      |nop +0
      |acc +1
      |jmp +4
      |acc +3
      |jmp -3
      |acc -99
      |acc +1
      |jmp -4
      |acc +6
    """.stripMargin))
  for (i <- test1.instructions)
    println(i)
  println("part 1")
  println(test1.run())
  println("part 2")
  for (p <- test1.genVariants())
    println(s"${p.run()}")

  println()
  println("--- input ---")
  val input = Program.parse(Utils.readResource("/day08/input.txt"))
  println("part 1")
  println(input.run())
  println("part 2")
  println(input.genVariants().map(_.run).filter(_.state == State.Terminated).toList)
}
