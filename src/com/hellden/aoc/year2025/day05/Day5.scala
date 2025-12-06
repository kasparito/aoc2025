package com.hellden.aoc.year2025.day05

import com.hellden.aoc.year2025.*
import com.hellden.aoc.year2025.Day.*

import scala.annotation.tailrec

object Day5 extends Day[BigInt](5):
  override val answers: List[List[BigInt]] = List(
    List(3, 14),
    List(558, 344813017450467L)
  )

case class Day5(input: String) extends SolutionFull[BigInt]:

  private val (ranges, numbers) =
    val lines = input.asLines
    val blank = lines.indexOf("")
    val r = lines.slice(0, blank).map:
      case s"$start-$stop" => (BigInt(start), BigInt(stop))
    val n = lines.slice(blank + 1, lines.length).map(BigInt(_))
    (r.toSet, n.toSet)

  override def part1: BigInt =
    numbers.count: n =>
      ranges.exists: (start, stop) =>
        n >= start && n <= stop

  override def part2: BigInt =
    countNumbers(ranges.toList.sorted)

  @tailrec
  private def countNumbers(ranges: List[(BigInt, BigInt)], acc: BigInt = 0): BigInt =
    ranges match
      case Nil =>
        acc
      case (start1, stop1) :: (start2, stop2) :: tail if start2 <= stop1 =>
        countNumbers((start1, stop1.max(stop2)) :: tail, acc)
      case (start, stop) :: tail =>
        countNumbers(tail, acc + (stop - start + 1))
