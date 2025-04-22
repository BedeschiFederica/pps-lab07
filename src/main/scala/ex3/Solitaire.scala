package ex3

object Solitaire extends App:
  private type Number = (Int, Int)
  private type Solution = Seq[Number]
  private type IterableFactory = Solution => Iterable[Solution]
  private val width = 5
  private val height = 5
  given IterableFactory = LazyList(_)

  @main def run(): Unit = placeNumbers().zipWithIndex foreach render

  def placeNumbers(n: Int = width * height)(using factory: IterableFactory): Iterable[Solution] = n match
    case 1 => factory(Seq((width / 2, height / 2)))
    case _ =>
      for
        numbers <- placeNumbers(n - 1)
        x <- 0 until width
        y <- 0 until height
        number = (x, y)
        if canReach(number, numbers.last) && notOccupied(number, numbers)
      yield
        numbers :+ number

  private def verticals(n: Number): Seq[Number] = Seq((n._1 - 3, n._2), (n._1 + 3, n._2))
  private def horizontals(n: Number): Seq[Number] = Seq((n._1, n._2 - 3), (n._1, n._2 + 3))
  private def diagonals(n: Number): Seq[Number] =
    Seq((n._1 - 2, n._2 - 2), (n._1 - 2, n._2 + 2), (n._1 + 2, n._2 - 2), (n._1 + 2, n._2 + 2))

  private def canReach(number: Number, from: Number): Boolean =
    verticals(from).concat(horizontals(from)).concat(diagonals(from)).contains(number)

  private def notOccupied(number: Number, numbers: Solution): Boolean = !numbers.contains(number)

  def render(si: (Solution, Int)): Unit =
    println()
    println(s"sol ${si._2}")
    val rows =
      for
        y <- 0 until height
        row =
          for
            x <- 0 until width
            number = si._1.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    println(rows.mkString("\n"))