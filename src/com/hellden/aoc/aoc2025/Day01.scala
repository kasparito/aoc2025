package com.hellden.aoc.aoc2025

import scala.math.BigInt.int2bigInt

object Day01 extends Day(1):

  private def countZeros(f: (direction: BigInt, zeros: BigInt) => BigInt): BigInt =
    inputLines
      .map:
        case s"L$n" => -BigInt(n)
        case s"R$n" => BigInt(n)
      .foldLeft((direction = BigInt(50), count = BigInt(0))):
        case ((direction, count), rotation) =>
          val newDirection = direction + rotation
          val normalizedDirection = newDirection.mod(100)
          val zeros =
            if direction == 0 || rotation >= 0 then
              newDirection.abs / 100
            else
              (100 - newDirection) / 100
          (normalizedDirection, count + f(normalizedDirection, zeros))
      .count

  override def part1: BigInt = // 997
    countZeros:
      case (0, _) => 1
      case _ => 0

  override def part2: BigInt = // 5978
    countZeros:
      (_, zeros) => zeros
