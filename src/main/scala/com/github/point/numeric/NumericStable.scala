package com.github
package point
package numeric

import scala.language.{existentials, higherKinds, reflectiveCalls}

/**
	* Created by Nicolas on 24/03/2017.
	*/
abstract class NumericStable[T : Numeric] extends Stable[Numeric, T] {
	class PointOfNumeric extends NumericPoint with Numeric[Repr] {
		class NumericPointOps(protected val lhs: Repr) extends NumericOps(lhs) with NumericPointScalarOps
	}

	object PointOfNumeric {
		def pointOfNumeric[T, Repr[X] <: point.Point[X] : NumericStable.Of[T]#For]: NumericStable.OfFor[T, Repr[T]]#PointOfNumeric = {
			val n = implicitly[NumericStable.OfFor[T, Repr[T]]]
			new n.PointOfNumeric
		}
	}
}

object NumericStable {
	type Of[T] = {
		type For[Repr[X] <: Point[X]] = OfFor[T, Repr[T]]
		type PointOfNumeric[Repr[X] <: Point[X]] = For[Repr]#PointOfNumeric
	}
	type OfFor[T, _Repr <: Point[T]] = NumericStable[T] {
		type Repr = _Repr
	}

	implicit def stableToNumericStable[T : Numeric, Repr[X] <: Point[X] : StableCompanion.For]: NumericStable.Of[T]#For[Repr] = {
		val companion = implicitly[StableCompanion.For[Repr]]
		type ReprOfT = Repr[T]
		new NumericStable[T] {
			type Repr = ReprOfT
			def fromSeq(seq: Seq[T]): Repr = companion.fromSeq(seq)
		}
	}

	abstract class NumericPoint[M[X] <: Numeric[X], T : M, Repr <: Point[T]] {point =>
		def fromSeq(list: Seq[T]): Repr
		val isNumeric: Numeric[T] = implicitly[Numeric[T]]

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

	abstract class PointOfNumeric[T : Numeric, Repr <: point.Point[T]] extends NumericPoint[Numeric, T, Repr] with Numeric[Repr] {
		class NumericPointOps(protected val lhs: Repr) extends NumericOps(lhs) with NumericPointScalarOps
	}

	object PointOfNumeric {
		def pointOfNumeric[T : Numeric, Repr[X] <: point.Point[X] : NumericStable.Of[T]#For]: PointOfNumeric[T, Repr[T]] = {
			val n = implicitly[NumericStable.OfFor[T, Repr[T]]]
			new PointOfNumeric[T, Repr[T]] {
				def fromSeq(list: Seq[T]) = n.fromSeq(list)
			}
		}
	}
}

/*Kept example as it illustrates a technique to work around overload and partial shadowing
abstract class NumericStable[T] extends Companion {
	//...

	def fromList(list: List[T])(implicit num: Numeric[T]): Repr[T]
	//Takes precedences over super.fromList because
	// 1. it has an implicit, which ensures a different imprint after type erasure (otherwise, it is a name clash)
	// 2. the template is moved to the trait level (otherwise, it shadows the super function even if there is no implicit)

	//...
}
*/
