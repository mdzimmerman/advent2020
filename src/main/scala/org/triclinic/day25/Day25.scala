package org.triclinic.day25

object Day25 extends App {
  def transform(subject: Long, loopSize: Long) = {
    var n = 1L
    for (_ <- 1L to loopSize) {
      n = n * subject
      n = n % 20201227
    }
    n
  }

  def findLoopSize(cardPubkey: Long, doorPubKey: Long) = {
    var cardLoop: Option[Long] = None
    var doorLoop: Option[Long] = None
    val subject = 7L
    var n = 1L
    var i = 0
    while (cardLoop.isEmpty || doorLoop.isEmpty) {
      n = n * subject
      n = n % 20201227
      i += 1
      println(s"i=$i n=$n")
      if (n == cardPubkey)
        cardLoop = Some(i)
      else if (n == doorPubKey)
        doorLoop = Some(i)
    }
    println(s"cardLoop=${cardLoop.get} doorLoop=${doorLoop.get}")
    println(s"${transform(doorPubKey, cardLoop.get)}")
    println(s"${transform(cardPubkey, doorLoop.get)}")
  }

  findLoopSize(5764801, 17807724)
  findLoopSize(5290733, 15231938)
}
