package com.hellden.aoc.year2025.day06

import com.hellden.aoc.year2025.*
import com.hellden.aoc.year2025.Day.*
import com.hellden.collection.*

object Day6 extends Day[BigInt](6):
  override val answers: List[List[BigInt]] = List(
    List(4277556, 3263827),
    List(4309240495780L, 9170286552289L)
  )

case class Day6(input: String) extends SolutionFull[BigInt]:

  private val numberLines =
    input.asLines.init

  private val operators =
    input.asLines.last.split("\\s+").filterNot(_.isBlank).map:
      case "+" => (a: BigInt, b: BigInt) => a + b
      case "*" => (a: BigInt, b: BigInt) => a * b

  private def reduceSum(numbers: Seq[Seq[BigInt]]) =
    numbers.zip(operators).map((num, op) => num.reduce(op)).sum

  override def part1: BigInt =
    reduceSum(numberLines.map(_.split("\\s+").filterNot(_.isBlank).map(BigInt(_))).transpose)

  override def part2: BigInt =
    reduceSum(numberLines.transpose.map(_.mkString).splitWhen(_.isBlank).map(_.map(s => BigInt(s.trim))))
