package com
package github.point.d2.standard

import github.point
import point.{StableCompanion, d2}

import language.higherKinds


/**
	* A complex point requires a mandatory numeric base type
	*/
case class Point[T](x: T, y: T) extends d2.Point[T] {
	val companion = Point
	def fromSeq(seq: Seq[T]): Point[T] = Point.fromSeq(seq)
}

object Point extends d2.Point.Companion with StableCompanion {companion =>
	type Bound[T] = Point[T]
	override type Repr[T] = Point[T] //Obvious, but not to the compiler
	def apply[T](point: (T, T)): Point[T] = {val (x, y) = point; this(x, y)}
	def fromSeq[T](list: Seq[T]): Repr[T] = Point(list(0), list(1))
	object Implicits {
		implicit val point = companion
	}
}

