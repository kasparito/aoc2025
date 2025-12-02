package com.hellden.aoc.year2025

import java.nio.file.{Files, Path}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.jdk.CollectionConverters.*

object Day:

  extension (s: String)
    def asLines: IndexedSeq[String] =
      s.split('\n').toIndexedSeq

  extension [T](x: T)
    def log(s: String): T =
      println(s"$s: $x")
      x

  extension [T](f: Future[T])
    def await: T = Await.result(f, Duration.Inf)

trait Day[R](day: Int):

  private val directory: Path = Path.of(f"input/day$day%02d")

  private val examples: List[String] =
    Files.newDirectoryStream(directory, "example*.txt").asScala.map(Files.readString).toList

  private val input: String =
    Files.readString(directory.resolve("input.txt"))

  val answers: List[List[R]] = Nil

  def apply(input: String): SolutionFirst[R]

  private def solve(title: String, input: String, answers: List[R]): Unit =
    val solutionOne = apply(input)
    println(s"\n$title:\n – Part 1: ${result(solutionOne.part1, answers.headOption)}")
    solutionOne match
      case solutionBoth: SolutionFull[?] =>
        println(s" – Part 2: ${result(solutionBoth.part2, answers.tail.headOption)}")
      case _ =>

  private def result(solution: R, answer: Option[R]): String =
    answer match
      case None => s"❔ $solution"
      case Some(answer) if solution == answer => s"✅ $solution"
      case Some(answer) => s"❌ $solution != $answer"

  def main(args: Array[String]): Unit =
    println(s"Day $day:")
    examples.zipWithIndex.foreach: (example, index) =>
      solve(s"Example ${index + 1}", example, answers.slice(index, answers.length).headOption.getOrElse(Nil))
    solve(s"Result", input, answers.slice(examples.length, answers.length).headOption.getOrElse(Nil))
