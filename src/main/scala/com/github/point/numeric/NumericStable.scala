package com.github
package point
package numeric

import scala.language.{existentials, reflectiveCalls}

/**
	* Created by Nicolas on 24/03/2017.
	*/
abstract class NumericStable[T : Numeric] extends Stable[Numeric, T] {
	trait PointOfNumericImplem extends Numeric[Repr] with NumericPoint {
		class NumericPointOpsImplem(_lhs: Repr) extends NumericOps(_lhs) with NumericPointScalarOps {protected def lhs = _lhs}
		type NumericPointOps <: NumericPointOpsImplem
		def mkNumericOpsImplem(lhs: Repr): NumericPointOps
		final override def mkNumericOps(lhs: Repr) = mkNumericOpsImplem(lhs)
	}
	type PointOfNumeric <: PointOfNumericImplem
	val pointOfNumeric: PointOfNumeric
	type NumericPointOps = pointOfNumeric.NumericPointOps
}

object NumericStable {
	type Of[T] = {
		type For[Repr[X] <: Point[X]] = OfFor[T, Repr[T]]
		type PointOfNumeric[Repr[X] <: Point[X]] = For[Repr]#PointOfNumeric
		type NumericPointOps[Repr[X] <: Point[X]] = For[Repr]#NumericPointOps
	}
	type OfFor[T, _Repr <: Point[T]] = NumericStable[T] {
		type Repr = _Repr
	}
}
