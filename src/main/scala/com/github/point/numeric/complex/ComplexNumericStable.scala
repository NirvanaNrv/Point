package com.github
package point
package numeric.complex

import d2.Point

import scala.language.implicitConversions

/**
	* Created by Nicolas on 24/03/2017.
	*/
abstract class ComplexNumericStable[T : Numeric] extends numeric.NumericStable[T] {
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
		/*
		override protected def unproject(x: T): Repr = {
			val zero = implicitly[Numeric[T]].fromInt(0)
			fromSeq(x #:: LazyList.continually(zero))
		}
		*/

		trait NumericPointScalarOps extends super.NumericPointScalarOps {
			protected def lhs: Repr
			def norm = point.norm(lhs)
			def conjugate = point.conjugate(lhs)
			def unary_! = conjugate
			def ! = conjugate
			def i = rotate()
		}
	}
	class PointOfNumericImplem extends super.PointOfNumericImplem with NumericPoint {
		class NumericPointOpsImplem(_lhs: Repr) extends super.NumericPointOpsImplem(_lhs) with NumericPointScalarOps
		type NumericPointOps = NumericPointOpsImplem
		def mkNumericOpsImplem(lhs: Repr) = new NumericPointOpsImplem(lhs)
	}

	type PointOfNumeric <: PointOfNumericImplem
}
