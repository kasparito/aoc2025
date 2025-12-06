package com.hellden

import scala.annotation.tailrec

package object collection:

  extension [A](i: Iterable[A])

    def cross[B](s: Seq[B]): Iterable[(A, B)] =
      for
        e1 <- i
        e2 <- s
      yield (e1, e2)

    def splitWhen(f: A => Boolean): Seq[Seq[A]] =
      @tailrec
      def splitRec(remaining: Iterable[A], current: Seq[A] = Seq.empty, acc: Seq[Seq[A]] = Seq.empty): Seq[Seq[A]] =
        remaining.headOption match
          case None =>
            acc :++ Option.when(current.nonEmpty)(current)
          case Some(value) if f(value) =>
            splitRec(remaining.tail, Seq.empty, acc :++ Option.when(current.nonEmpty)(current))
          case Some(value) =>
            splitRec(remaining.tail, current :+ value, acc)
      splitRec(i)

  extension [T](s: Seq[T])
    def middle: T =
      if s.length % 2 == 0 then
        throw new NoSuchElementException(s"middle of even sized (${s.length}) sequence")
      else
        s(s.length / 2)

  extension [K, V](pairs: Iterable[(K, V)])
    def groupKeyValue: Map[K, Iterable[V]] =
      pairs.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
