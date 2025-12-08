package com.hellden.aoc.year2025.day08

import com.hellden.aoc.year2025.*
import com.hellden.aoc.year2025.Day.*
import com.hellden.aoc.year2025.day08.Day8.{Pair, Point}

import scala.annotation.tailrec
import scala.collection.SortedSet
import scala.math.BigDecimal.*

object Day8 extends Day[BigInt](8):
  override val answers: List[List[BigInt]] = List(
    List(40, 25272),
    List(63920, 1026594680)
  )

  case class Point(x: Double, y: Double, z: Double):
    def distance(p: Point): Double =
      math.sqrt(math.pow(x - p.x, 2) + math.pow(y - p.y, 2) + math.pow(z - p.z, 2))

  case class Pair(p1: Point, p2: Point):
    val distance: Double = p1.distance(p2)

  object Pair:
    given Ordering[Pair] = Ordering.by(_.distance)

case class Day8(input: String) extends SolutionFull[BigInt]:

  private val (pairs, clusters) =
    val points = input.asLines.map { case s"$x,$y,$z" => Point(x.toDouble, y.toDouble, z.toDouble) }
    val pairs = SortedSet(points.flatMap(p1 => points.collect { case p2 if p1 != p2 => Pair(p1, p2) })*).toList
    val clusters = points.map(p => p -> Set(p)).toMap
    (pairs, clusters)

  override def part1: BigInt =
    pairs.take(1000).foldLeft(clusters)(merge).values.toSeq.distinct.map(_.size).sorted.takeRight(3).product

  private def merge(clusters: Map[Point, Set[Point]], pair: Pair): Map[Point, Set[Point]] =
    val mergedCluster = clusters(pair.p1) ++ clusters(pair.p2)
    clusters ++ mergedCluster.map(_ -> mergedCluster)

  override def part2: BigInt =
    val Pair(p1, p2) = findLastJunctionBoxPair(clusters, pairs)
    p1.x.toBigInt * p2.x.toBigInt

  @tailrec
  private def findLastJunctionBoxPair(clusters: Map[Point, Set[Point]], pairs: List[Pair]): Pair =
    merge(clusters, pairs.head) match
      case c if c.values.toSet.size == 1 => pairs.head
      case c => findLastJunctionBoxPair(c, pairs.tail)
