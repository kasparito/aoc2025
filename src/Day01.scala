import scala.io.Source

object Day01 extends App:

  val input: String =
    val source = Source.fromFile(f"input/day01.txt")
    try source.mkString finally source.close()

  lazy val inputLines: IndexedSeq[String] =
    input.split('\n').toIndexedSeq

  def part1: Int =
    inputLines
      .map:
        case s"L$n" => -n.toInt
        case s"R$n" => n.toInt
      .foldLeft((direction = 50, count = 0)):
        case ((direction, count), rotation) =>
          val newDirection = (direction + rotation) % 100
          if newDirection == 0 then
            (newDirection, count + 1)
          else
            (newDirection, count)
      .count

  def part2: Int =
    inputLines
      .map:
        case s"L$n" => -n.toInt
        case s"R$n" => n.toInt
      .foldLeft((direction = 50, count = 0)):
        case ((direction, count), rotation) =>
          (
            (100 + direction + rotation % 100) % 100,
            count + {
              val diff =
                if rotation < 0 && direction > 0 then
                  -((direction - 100) + rotation) / 100
                else
                  math.abs((direction + rotation) / 100)
              println(s"$direction + $rotation = $diff")
              diff
            }
          )
      .count

  println(part1)
  println(part2)
