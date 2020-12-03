import scala.io.Source

case class Forest(lines: Seq[String]) {
  val width = lines.map(_.length).max
  val height = lines.length
  val arr = buildArray()
  //println(s"w=$width h=$height")

  private def buildArray(): Array[Array[Char]] = {
    val arr = Array.ofDim[Char](width, height)
    for ((line, j) <- lines.zipWithIndex)
      for ((c, i) <- line.zipWithIndex)
        arr(i)(j) = c
    arr
  }

  def getGrid(): String = {
    val s = new StringBuilder
    for (j <- 0 until height) {
      for (i <- 0 until width)
        s += arr(i)(j)
      s ++= "\n"
    }
    s.toString()
  }

  def countSlope(x: Int, y: Int): Long = {
    (for (i <- 0 until height/y) yield (x * i, y * i))
      .filter{case (x, y) => arr(x % width)(y) == '#'}
      .length.toLong
  }
}

val test1 = Forest("""
 |..##.......
 |#...#...#..
 |.#....#..#.
 |..#.#...#.#
 |.#...##..#.
 |..#.##.....
 |.#.#.#....#
 |.#........#
 |#.##...#...
 |#...##....#
 |.#..#...#.#
""".stripMargin.split("""\n""").map(_.trim).filter(_.nonEmpty).toList)

println(test1.getGrid())
println(test1.countSlope(3, 1))
val testslopes = List(
  test1.countSlope(1, 1),
  test1.countSlope(3, 1),
  test1.countSlope(5, 1),
  test1.countSlope(7, 1),
  test1.countSlope(1, 2)
)
println(testslopes.product)
println()

val input = Forest(Source.fromFile("input.txt").getLines.toSeq)
println(input.countSlope(3, 1))
val inputslopes = List(
  input.countSlope(1, 1),
  input.countSlope(3, 1),
  input.countSlope(5, 1),
  input.countSlope(7, 1),
  input.countSlope(1, 2)
)
println(inputslopes)
println(inputslopes.product)