package com.hellden.collection

import scala.annotation.tailrec

object BigIntRange:

  given Ordering[BigIntRange] =
    Ordering.by: range =>
      (range.start, range.stop)

  def parse(range: String): BigIntRange =
    range match
      case s"$start-$stop" =>
        BigIntRange(BigInt(start), BigInt(stop))

  def merge(ranges: Iterable[BigIntRange]): Iterable[BigIntRange] =
    @tailrec
    def merge(ranges: List[BigIntRange], acc: List[BigIntRange]): Iterable[BigIntRange] = ranges match
      case Nil =>
        acc
      case range1 :: range2 :: tail if range1.stop + 1 >= range2.start =>
        merge(BigIntRange(range1.start, range1.stop.max(range2.stop)) :: tail, acc)
      case head :: tail =>
        merge(tail, head :: acc)
    merge(ranges.toList.sorted, Nil)

case class BigIntRange(start: BigInt, stop: BigInt):

  def contains(n: BigInt): Boolean =
    n >= start && n <= stop

  def size: BigInt =
    stop - start + 1
