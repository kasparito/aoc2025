package com.hellden.aoc.year2025.day07

import com.hellden.aoc.year2025.*
import com.hellden.aoc.year2025.Day.*

object Day7 extends Day[BigInt](7):
  override val answers: List[List[BigInt]] = List(
    List(21, 40),
    List(1566, 5921061943075L)
  )

case class Day7(input: String) extends SolutionFull[BigInt]:

  private val start = input.indexOf('S')
  private val splitters = input.asLines.map: line =>
    line.zipWithIndex.collect { case ('^', index) => index }.toSet

  override def part1: BigInt =
    splitters
      .foldLeft((beams = Set(input.indexOf('S')), splitCount = 0)):
        case ((beams, splitCount), splits) =>
          val splitBeams = beams.flatMap: beam =>
            if splits(beam) then Set(beam - 1, beam + 1) else Set(beam)
          (splitBeams, splitCount + beams.count(splits))
      .splitCount

  override def part2: BigInt =
    splitters
      .foldLeft[Map[Int, BigInt]](Map(input.indexOf('S') -> 1)):
        case (beamTimelines, splits) =>
          val splitBeams = beamTimelines.view.flatMap: (beam, timelines) =>
            if splits(beam) then Map(beam - 1 -> timelines, beam + 1 -> timelines) else Map(beam -> timelines)
          splitBeams.groupMapReduce(_._1)(_._2)(_ + _)
      .values
      .sum
