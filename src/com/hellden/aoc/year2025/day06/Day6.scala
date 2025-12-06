package com.hellden.aoc.year2025.day06

import com.hellden.aoc.year2025.*
import com.hellden.aoc.year2025.Day.*

import scala.annotation.tailrec

object Day6 extends Day[BigInt](6):
  override val answers: List[List[BigInt]] = List(
    List(4277556, 3263827),
    List(4309240495780L, 9170286552289L)
  )

case class Day6(input: String) extends SolutionFull[BigInt]:

  override def part1: BigInt =
    val lines = input.asLines
    val operators = lines.last.split("\\s+").filterNot(_.isBlank).map:
      case "+" => (a: BigInt, b: BigInt) => a + b
      case "*" => (a: BigInt, b: BigInt) => a * b
    val numbers = lines.init.map: line =>
      line.split("\\s+").filterNot(_.isBlank).map(BigInt(_))
    operators
      .zipWithIndex
      .map: (op, ix) =>
        numbers.map(_(ix)).reduce(op)
      .sum

  override def part2: BigInt =
    val lines = input.asLines
    val operators = lines.last.reverse.split("\\s+").filterNot(_.isBlank).map:
      case "+" => (a: BigInt, b: BigInt) => a + b
      case "*" => (a: BigInt, b: BigInt) => a * b
    summarize(lines.map(_.length).max, lines.init, Nil, 0, operators)

  @tailrec
  private def summarize(pos: Int, lines: IndexedSeq[String], acc: List[BigInt], sum: BigInt, operators: IndexedSeq[(BigInt, BigInt) => BigInt]): BigInt =
    if pos < 0 || operators.isEmpty then
      sum + acc.reduce(operators.head)
    else
      lines.map(_.applyOrElse(pos, _ => ' ')).mkString match
        case s if s.isBlank && acc.isEmpty =>
          summarize(pos - 1, lines, Nil, sum, operators)
        case s if s.isBlank =>
          summarize(pos - 1, lines, Nil, sum + acc.reduce(operators.head), operators.tail)
        case s =>
          summarize(pos - 1, lines, BigInt(s.trim) :: acc, sum, operators)
