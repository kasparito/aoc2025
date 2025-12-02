package com.hellden.aoc.aoc2025

object Day02 extends Day(2):

  private val ids = inputLines.mkString.split(',').toSet.flatMap:
    case s"$start-$stop" =>
      BigInt(start) to BigInt(stop)

  override def part1: BigInt = // 38158151648
    ids.filter(n => hasRepeatedNumber(n.toString, 2)).sum

  override def part2: BigInt = // 45283684555
    ids.filter(n => hasRepeatedNumber(n.toString)).sum

  private def hasRepeatedNumber(s: String): Boolean =
    (2 to s.length).exists(hasRepeatedNumber(s, _))

  private def hasRepeatedNumber(s: String, count: Int): Boolean =
    s.length >= count && s.grouped(s.length / count).distinct.size == 1
