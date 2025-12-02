package com.hellden.aoc.year2025.day02

import com.hellden.aoc.year2025.*
import com.hellden.aoc.year2025.Day.*

import scala.math.BigDecimal.*

object Solution extends Day[BigInt](2):

  override val answers: List[List[BigInt]] = List(
    List(1227775554.toBigInt, 4174379265L.toBigInt),
    List(38158151648L.toBigInt, 45283684555L.toBigInt)
  )

case class Solution(input: String) extends SolutionFull[BigInt]:

  private val ids = input.asLines.mkString.split(',').toSet.flatMap:
    case s"$start-$stop" =>
      BigInt(start) to BigInt(stop)

  override def part1: BigInt =
    ids.filter(n => hasRepeatedNumber(n.toString, 2)).sum

  override def part2: BigInt =
    ids.filter(n => hasRepeatedNumber(n.toString)).sum

  private def hasRepeatedNumber(s: String): Boolean =
    Range.inclusive(2, s.length).exists(hasRepeatedNumber(s, _))

  private def hasRepeatedNumber(s: String, count: Int): Boolean =
    s.length >= count && s.grouped(s.length / count).distinct.size == 1
