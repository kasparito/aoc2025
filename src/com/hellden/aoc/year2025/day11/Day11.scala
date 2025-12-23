package com.hellden.aoc.year2025.day11

import com.hellden.aoc.year2025.*
import com.hellden.aoc.year2025.Day.*
import com.hellden.grid.*
import com.hellden.grid.Direction.*
import com.hellden.number.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.Ordered.*

object Day11 extends Day[Long](11):
  override val answers: List[List[Long]] = List(
    List(5, 2),
    List(764)
  )

case class Day11(input: String) extends SolutionFull[Long]:

  private val connections = input
    .asLines
    .map: line =>
      line.split(":? ").toList match
        case device :: outputs =>
          device -> outputs
        case _ =>
          throw IllegalArgumentException(s"Invalid line: $line")
    .toMap

  override def part1: Long = countPaths(List("you"), Set.empty, 0)

  @tailrec
  private def countPaths(devices: List[String], visited: Set[String], count: Long): Long =
    devices match
      case Nil =>
        count
      case "out" :: remaining =>
        countPaths(remaining, visited, count + 1)
      case device :: remaining =>
        countPaths(
          remaining ++ connections.getOrElse(device, Nil).filterNot(visited.contains),
          visited + device,
          count
        )

  override def part2: Long = paths(List("svr"), Map.empty).count(identity)

  @tailrec
  private def paths(toVisit: List[String], visited: Map[String, List[Boolean]]): List[Boolean] =
    toVisit match
      case Nil =>
        visited.get("out").toList.flatten
      case "out" :: remaining =>
        paths(remaining, visited + ("out" -> (true :: visited.getOrElse("out", Nil))))
      case device :: remaining =>
        paths(
          remaining ++ connections.getOrElse(device, Nil).filterNot(visited.contains),
          visited + (device -> (false :: visited.getOrElse(device, Nil)))
        )
