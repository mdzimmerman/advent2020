package org.triclinic

import java.io.{ByteArrayInputStream, InputStream}
import scala.annotation.tailrec
import scala.io.Source

object Utils {
  def readResource(path: String) = {
    val stream: InputStream = getClass.getResourceAsStream(path)
    Source.fromInputStream(stream).getLines.toSeq
  }

  def readResourceString(path: String) = {
    val stream: InputStream = getClass.getResourceAsStream(path)
    Source.fromInputStream(stream).mkString
  }

  def readString(s: String): Seq[String] = {
    val stream: InputStream = new ByteArrayInputStream(s.getBytes)
    Source.fromInputStream(stream).getLines.toSeq
  }

  @tailrec
  private def split[T](xs: List[T],
                       delim: T,
                       curr: List[T],
                       out: List[List[T]]): List[List[T]] = {
    if (xs.isEmpty) {
      (curr.reverse :: out).reverse
    } else {
      val (currNew, outNew) = if (xs.head == delim)
        (Nil, curr.reverse :: out)
      else
        (xs.head :: curr, out)
      split[T](xs.tail, delim, currNew, outNew)
    }
  }

  def split[T](xs: List[T],
               delim: T): List[List[T]] = split[T](xs, delim, Nil, Nil)
}

object AsInt {
  def unapply(s: String): Option[Int] = try {
    Some(s.toInt)
  } catch {
    case _: java.lang.Exception => None
  }
}

object AsLong {
  def unapply(s: String): Option[Long] = try {
    Some(s.toLong)
  } catch {
    case _: java.lang.Exception => None
  }
}
