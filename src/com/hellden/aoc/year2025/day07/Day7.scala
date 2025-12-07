package com.hellden.aoc.year2025.day07

import com.hellden.aoc.year2025.*
import com.hellden.aoc.year2025.Day.*

object Day7 extends Day[BigInt](7):
  override val answers: List[List[BigInt]] = List(
    List(21, 40),
    List(1566, 5921061943075L)
  )

case class Day7(input: String) extends SolutionFull[BigInt]:

  private val result = input.asLines.foldLeft((beamTimelines = Map(input.indexOf('S') -> 1L), splitCount = 0)):
    case ((beamTimelines, splitCount), line) =>
      val splits = line.zipWithIndex.collect { case ('^', index) => index }.toSet
      val splitBeams = beamTimelines.view.flatMap: (beam, timelines) =>
        if splits(beam) then Map(beam - 1 -> timelines, beam + 1 -> timelines) else Map(beam -> timelines)
      (splitBeams.groupMapReduce(_._1)(_._2)(_ + _), splitCount + beamTimelines.keys.count(splits))

  override def part1: BigInt = result.splitCount
  override def part2: BigInt = result.beamTimelines.values.sum
