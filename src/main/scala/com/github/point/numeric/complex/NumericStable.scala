package com.github
package point.numeric.complex

import point.{HigherKinded, d2}
import d2.Point

/**
	* Created by Nicolas on 24/03/2017.
	*/
abstract class NumericStable[T : Numeric] extends point.numeric.NumericStable[T] {
	type Repr <: Point[T]

	trait NumericPoint extends super.NumericPoint {point =>
		override def times(x: Repr, y: Repr): Repr = {
			import isNumeric._
			fromSeq(List(x._1 * y._1 - x._2 * y._2, x._1 * y._2 + x._2 * y._1))
		}
		def norm(x: Repr) = {
			import isNumeric._
			x.toList.map(c => c * c).sum
		}
		def conjugate(x: Repr) = x.updated(1, isNumeric.negate(x._2))

		//protected def complexLift(f: (Repr, Repr) => Repr)(x: Repr, y: T): Repr = ??? //f(x, x.zero.updated(0, y))
		override protected def unproject(x: T): Repr = {
			val zero = implicitly[Numeric[T]].fromInt(0)
			fromSeq(x #:: LazyList.continually(zero))
		}

		trait NumericPointScalarOps extends super.NumericPointScalarOps {
			protected def lhs: Repr
			def norm = point.norm(lhs)
			def conjugate = point.conjugate(lhs)
			def unary_! = conjugate
			def i = rotate()
		}
	}

	class PointOfNumeric extends Numeric[Repr] with NumericPoint {
		class NumericPointOps(protected val lhs: Repr) extends NumericOps(lhs) with NumericPointScalarOps
	}
}

object NumericStable extends HigherKinded {
	trait Of[T] {
		type For[Repr[X] <: Point[X]] = OfFor[T, Repr[T]]
		type PointOfNumeric[Repr[X] <: Point[X]] = For[Repr]#PointOfNumeric
	}
	type OfFor[T, _Repr <: Point[T]] = NumericStable[T] {
		type Repr = _Repr
	}
}
