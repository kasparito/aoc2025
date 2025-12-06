package com.hellden.aoc.year2025.day05

import com.hellden.aoc.year2025.*
import com.hellden.aoc.year2025.Day.*
import com.hellden.collection.*

object Day5 extends Day[BigInt](5):
  override val answers: List[List[BigInt]] = List(
    List(3, 14),
    List(558, 344813017450467L)
  )

case class Day5(input: String) extends SolutionFull[BigInt]:

  private val (ranges, numbers) =
    input.asLines.splitWhen(_.isBlank) match
      case Seq(ranges, numbers) =>
        (ranges.map(BigIntRange.parse).toSet, numbers.map(BigInt(_)).toSet)

  override def part1: BigInt =
    numbers.count(n => ranges.exists(_.contains(n)))

  override def part2: BigInt =
    BigIntRange.merge(ranges).map(_.size).sum
