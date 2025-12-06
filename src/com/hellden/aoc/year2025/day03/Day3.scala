package com.hellden.aoc.year2025.day03

import com.hellden.aoc.year2025.*
import com.hellden.aoc.year2025.Day.*

object Day3 extends Day[BigInt](3):
  override val answers: List[List[BigInt]] = List(
    List(357, 3121910778619L),
    List(17443, 172167155440541L)
  )

case class Day3(input: String) extends SolutionFull[BigInt]:

  private val batteryBanks: Seq[String] = input.asLines

  override def part1: BigInt = totalOutputJoltage(2)

  override def part2: BigInt = totalOutputJoltage(12)

  private def totalOutputJoltage(n: Int): BigInt = batteryBanks.flatMap(maxJoltage(_, n)).sum

  private def maxJoltage(bank: String, take: Int, acc: Seq[Char] = Seq.empty): Option[BigInt] =
    if take == 0 then
      Some(BigInt(acc.mkString))
    else
      bank
        .zipWithIndex
        .sortBy((char, index) => (-char, index))
        .view
        .flatMap: (c, index) =>
          maxJoltage(bank.substring(index + 1), take - 1, acc :+ c)
        .headOption
