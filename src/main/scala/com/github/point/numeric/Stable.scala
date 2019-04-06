package com.github
package point
package numeric

import language.higherKinds

abstract class Stable[M[X] <: Numeric[X], T : M] {
	type Repr <: Point[T]
	val isNumeric: Numeric[T] = implicitly[Numeric[T]]

	def fromSeq(list: Seq[T]): Repr

	trait NumericPoint {point =>
		protected def lift(op: M[T] => (T, T) => T)(x: T, y: T) = op(implicitly[M[T]])(x, y)
		protected def opLift(op: M[T] => (T, T) => T)(x: Repr, y: Repr): Repr =
			fromSeq((x.toList zip y.toList).map(p => lift(op)(p._1, p._2)))
		protected def unproject(x: T): Repr = fromSeq(LazyList.continually(x))
		protected def project(x: Repr): T = x._1
		protected def scalarLift(op: M[T] => (T, T) => T)(x: Repr, y: T): Repr = opLift(op)(x, unproject(y))
		protected final def unaryLift(f: T => T)(x: Repr): Repr = fromSeq(x.toList.map(f))
		protected final def fromHead[U](f: M[T] => T => U)(x: Repr) = f(implicitly[M[T]])(project(x))
		def plus(x: Repr, y: Repr): Repr = opLift(_.plus)(x, y)
		def minus(x: Repr, y: Repr): Repr = opLift(_.minus)(x, y)
		def times(x: Repr, y: Repr): Repr = opLift(_.times)(x, y)
		def negate(x: Repr): Repr = unaryLift(isNumeric.negate)(x)
		def fromInt(x: Int): Repr = {
			val zero = implicitly[M[T]].fromInt(0)
			fromSeq(LazyList.continually(zero))
		}
		def parseString(str: String): Option[Repr] = None
		def toInt(x: Repr): Int = fromHead(_.toInt)(x)
		def toLong(x: Repr): Long = fromHead(_.toLong)(x)
		def toFloat(x: Repr): Float = fromHead(_.toFloat)(x)
		def toDouble(x: Repr): Double = fromHead(_.toDouble)(x)
		def scalarPlus(x: Repr, y: T): Repr = scalarLift(_.plus)(x, y)
		def scalarMinus(x: Repr, y: T): Repr = scalarLift(_.minus)(x, y)
		def scalarTimes(x: Repr, y: T): Repr = scalarLift(_.times)(x, y)
		def rotate(x: Repr, i: Int = 0, j: Int = 1): Repr = {
			require(i != j, s"\nYou must identify a plane, so the two indices must be different:\n x (=$i) must not be y (=$j) in (x, y)")
			fromSeq(x.toList.updated(i, isNumeric.negate(x(j))).updated(j, x(i)))
		}

		def compare(x: Repr, y: Repr) = {
			Ordering.Iterable(isNumeric).compare(x.toList, y.toList)
			import Ordering.Implicits._
			implicit val ordering: Ordering[T] = isNumeric
			val listOrdering = implicitly[Ordering[List[T]]]
			listOrdering.compare(x.toList, y.toList)
		}

		trait NumericPointScalarOps {
			protected def lhs: Repr
			def +(rhs: T): Repr = point.scalarPlus(lhs, rhs)
			def -(rhs: T): Repr = point.scalarMinus(lhs, rhs)
			def *(rhs: T): Repr = point.scalarTimes(lhs, rhs)
			def rotate(i: Int = 0, j: Int = 1): Repr = point.rotate(lhs, i, j)
		}
	}
}
