package com.hellden.aoc.year2025.day03

import com.hellden.aoc.year2025.*
import com.hellden.aoc.year2025.Day.*

import scala.math.BigDecimal.*

object Solution extends Day[BigInt](3):
  override val answers: List[List[BigInt]] = List(
    List(357.toBigInt, 3121910778619L.toBigInt),
    List(17443.toBigInt, 172167155440541L.toBigInt)
  )

case class Solution(input: String) extends SolutionFull[BigInt]:

  private def max(bank: String, acc: List[Char], take: Int): Option[String] =
    if take == 0 then
      Some(acc.mkString)
    else
      bank
        .zipWithIndex
        .sortBy((char, _) => -char)
        .view
        .flatMap: (c, index) =>
          max(bank.substring(index + 1), acc :+ c, take - 1)
        .headOption

  override def part1: BigInt =
    input.asLines.flatMap(max(_, Nil, 2)).map(BigInt(_)).sum

  override def part2: BigInt =
    input.asLines.flatMap(max(_, Nil, 12)).map(BigInt(_)).sum
