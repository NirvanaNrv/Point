package com.github.point
package d2.complex

import Point.Bound
import numeric.PairNumeric
import numeric.complex.{FractionalStable, IntegralStable, NumericStable}

/**
	* Created by Nicolas on 15/03/2017.
	*/
case class Point[T](x: T, y: T)(implicit ev: Numeric[T]) extends d2.Point[T] {
	val companion = Point
	override def fromSeq(list: Seq[T]): Point[T] = companion.fromSeq(list)
	override def updated(i: Int, value: T): Point[T] = fromSeq(toList.updated(i, value))
	def map[U](f: T => U)(implicit ev: Numeric[U]): Point[U] = companion.fromSeq(toList.map(f))
	override def reverse: Point[T] = fromSeq(toList.reverse)
	def zip[U](point: Bound[U])(implicit evU: Numeric[U]): Point[(T, U)] = {
		//For that kind of overload to compile, we need
		// 1. a neat expected return type, seemingly different enough from the overloaded function's one
		// 2. a implicit in scope; otherwise, it raises an error about ambiguous overload, not about missing implicit
		// 3. and in that case, for the implicit to be computed, a distinct name for the implicit values in scope,
		//	as it proved to be breaking to let a normally harmless shadowing happen (between an attribute and a parameter)
		//TODO declare a bug in Scala for that unexpected shadowing drawback
		import companion.InternalImplicits.makePairNumeric
		companion.fromSeq(toList.zip(point.toList))
	}
	override def toString = {
		import ev._
		val re = if (x != zero) s"$x" else ""
		val im = if (y != zero) if (y != one) if (y != -one) s"${y}i" else "-i" else "i" else ""
		val sep = if (x != zero) if (y > zero) "+" else "" else ""
		s"$re$sep$im"
	}
}

object Point extends d2.Point.Companion {companion =>
	type Bound[T] = d2.Point[T]
	type Repr[T] = Point[T]
	def apply[T](point: (T, T))(implicit ev: Numeric[T]): Point[T] = {val (x, y) = point; this(x, y)} //As quickly written as extracting types for .tupled
	def fromSeq[T](list: Seq[T]): Bound[T] = d2.standard.Point(list(0), list(1))
	def fromSeq[T](list: Seq[T])(implicit ev: Numeric[T]): Point[T] = this(list(0), list(1))
	object InternalImplicits {
		implicit def makePairNumeric[T, U](implicit evT: Numeric[T], evU: Numeric[U]): Numeric[(T, U)] = new PairNumeric
	}
	object Implicits {
		implicit def toNumericStable[T : Numeric]: numeric.complex.NumericStable.OfFor[T, Repr[T]] = {
			type ReprOfT = Repr[T]
			new NumericStable {
				type Repr = ReprOfT
				def fromSeq(seq: Seq[T]): Repr = companion.fromSeq(seq)
			}
		}

		implicit def toIntegralStable[T : Integral]: numeric.complex.IntegralStable.OfFor[T, Repr[T]] = {
			type ReprOfT = Repr[T]
			new IntegralStable {
				type Repr = ReprOfT
				def fromSeq(seq: Seq[T]): Repr = companion.fromSeq(seq)
			}
		}

		implicit def toFractionalStable[T : Fractional]: numeric.complex.FractionalStable.OfFor[T, Repr[T]] = {
			type ReprOfT = Repr[T]
			new FractionalStable {
				type Repr = ReprOfT
				def fromSeq(seq: Seq[T]): Repr = companion.fromSeq(seq)
			}
		}
	}
}
