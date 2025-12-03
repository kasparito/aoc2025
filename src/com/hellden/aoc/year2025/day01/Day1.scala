package com.hellden.aoc.year2025.day01

import com.hellden.aoc.year2025.*
import com.hellden.aoc.year2025.Day.*

object Day1 extends Day[BigInt](1):
  override val answers: List[List[BigInt]] = List(
    List(3, 6),
    List(997, 5978)
  )

case class Day1(input: String) extends SolutionFull[BigInt]:

  private val rotations = input.asLines.map:
    case s"L$n" => -BigInt(n)
    case s"R$n" => BigInt(n)

  override def part1: BigInt =
    countZeros:
      case (0, _) => 1
      case _ => 0

  override def part2: BigInt =
    countZeros:
      (_, zeros) => zeros

  private def countZeros(f: (direction: BigInt, zeros: BigInt) => BigInt): BigInt =
    rotations
      .foldLeft[(direction: BigInt, count: BigInt)]((50, 0)):
        case ((direction, count), rotation) =>
          val newDirection = direction + rotation
          val normalizedDirection = newDirection.mod(100)
          val zeros =
            if direction == 0 || rotation >= 0 then
              newDirection.abs / 100
            else
              (100 - newDirection) / 100
          (normalizedDirection, count + f(normalizedDirection, zeros))
      .count
