package com.hellden.aoc.year2025.day04

import com.hellden.aoc.year2025.*
import com.hellden.aoc.year2025.Day.*
import com.hellden.grid.{Direction, Position}

import scala.annotation.tailrec

object Day4 extends Day[BigInt](4):
  override val answers: List[List[BigInt]] = List(
    List(13, 43),
    List(1480, 8899)
  )

case class Day4(input: String) extends SolutionFull[BigInt]:

  private val initialState =
    for
      (line, y) <- input.asLines.zipWithIndex
      case ('@', x) <- line.zipWithIndex
    yield
      Position(x, y)
  .toSet

  override def part1: BigInt = removableRollsOfPaper(initialState).size

  override def part2: BigInt = removeRollsOfPaper(initialState)

  private def removableRollsOfPaper(state: Set[Position]): Set[Position] =
    state.filter: position =>
      4 > Direction.values.count: direction =>
        state.contains(position.moveIn(direction))

  @tailrec
  private def removeRollsOfPaper(state: Set[Position], count: BigInt = 0): BigInt =
    removableRollsOfPaper(state) match
      case removable if removable.isEmpty => count
      case removable => removeRollsOfPaper(state -- removable, count + removable.size)
