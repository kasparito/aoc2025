package com.hellden.aoc.year2025.day09

import com.hellden.aoc.year2025.*
import com.hellden.aoc.year2025.Day.*
import com.hellden.grid.*
import com.hellden.grid.Direction.*
import com.hellden.number.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.Ordered.*

object Day9 extends Day[Long](9):
  override val answers: List[List[Long]] = List(
    List(50, 24),
    List(4745816424L)
  )

case class Day9(input: String) extends SolutionFull[Long]:

  case class Rectangle(p1: Position, p2: Position):
    val area: Long = ((p1.x - p2.x).abs + 1L) * ((p1.y - p2.y).abs + 1L)

  private val redBricks = input.asLines.map:
    case s"$start,$stop" => Position(start.toInt, stop.toInt)

  private val rectangles = redBricks.flatMap: p1 =>
    redBricks.collect:
      case p2 if (p1.x, p1.y) < (p2.x, p2.y) => Rectangle(p1, p2)
      case p2 if (p1.x, p1.y) > (p2.x, p2.y) => Rectangle(p2, p1)

  override def part1: Long =
    rectangles.map(_.area).max

  override def part2: Long =
    val greenBricks = (Seq(redBricks.last, redBricks.head) :: redBricks.sliding(2).toList).flatMap:
      case Seq(p1, p2) if p1.x == p2.x =>
        (p1.y + (p2.y - p1.y).sign until p2.y by (p2.y - p1.y).sign).map(y => Position(p1.x, y))
      case Seq(p1, p2) if p1.y == p2.y =>
        (p1.x + (p2.x - p1.x).sign until p2.x by (p2.x - p1.x).sign).map(x => Position(x, p1.y))
    val bricks = (redBricks.map(_ -> '#') ++ greenBricks.map(_ -> 'O')).toMap
    val maxX = bricks.keys.map(_.x).max

    val memo = mutable.Map(bricks.keys.map(_ -> true).toSeq*)

    @tailrec
    def memoize(position: Position, inside: Boolean): Unit =
      memo.get(position) match
        case None if !inside =>
          memo.put(position, inside)
        case None if position.x <= maxX =>
          memo.put(position, inside)
          memoize(position.moveIn(E), inside)
        case _ =>
          ()

    rectangles
      .sortBy(p => -p.area)
      .take(100)
      .map:
        case rectangle@Rectangle(Position(x1, y1), Position(x2, y2)) =>
          val s = (x1 to maxX).map(xCheck => bricks.getOrElse(Position(xCheck, y1), '.')).mkString
          val inside = s.split('.').count(_.nonEmpty).odd
          (rectangle)
      .find:
        case Rectangle(Position(x1, y1), Position(x2, y2)) =>
          val verticalBounds = math.min(y1, y2) to math.max(y1, y2)
          val horizontalBounds = math.min(x1, x2) to math.max(x1, x2)
          verticalBounds.forall: y =>
            horizontalBounds.forall: x =>
              val position = Position(x, y)
              memo.getOrElse(position, {
                val s = (x to maxX).map(xCheck => bricks.getOrElse(Position(xCheck, y), '.')).mkString
                val inside = s.split('.').count(_.nonEmpty).odd
                memoize(position, inside)
                inside
              })
      .map(_.area)
      .max
