package org.triclinic

import java.io.{ByteArrayInputStream, InputStream}
import scala.io.Source

object Utils {
  def readResource(path: String) = {
    val stream: InputStream = getClass.getResourceAsStream(path)
    Source.fromInputStream(stream).getLines.toSeq
  }

  def readString(s: String): Seq[String] = {
    val stream: InputStream = new ByteArrayInputStream(s.getBytes)
    Source.fromInputStream(stream).getLines.toSeq
  }
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
