package com.github
package point
package numeric

/**
	* Created by Nicolas on 24/03/2017.
	*/
abstract class FractionalStable[T : Fractional] extends Stable[Fractional, T] {
	protected val isFractional: Fractional[T] = implicitly[Fractional[T]]

	trait FractionalPoint extends NumericPoint {point =>
		protected val isFractional = implicitly[Fractional[T]]
		def div(x: Repr, y: Repr): Repr = opLift(_.div)(x, y)
		def scalarDiv(x: Repr, y: T): Repr = scalarLift(_.div)(x, y)

		trait FractionalPointScalarOps extends NumericPointScalarOps {
			def /(rhs: T): Repr = scalarDiv(lhs, rhs)
		}
	}

	trait PointOfFractionalImplem extends Fractional[Repr] with FractionalPoint {
		class FractionalPointOpsImplem(_lhs: Repr) extends FractionalOps(_lhs) with FractionalPointScalarOps {protected def lhs = _lhs}
		type FractionalPointOps <: FractionalPointOpsImplem
		def mkFractionalOpsImplem(lhs: Repr): FractionalPointOps
		final override def mkNumericOps(lhs: Repr) = mkFractionalOpsImplem(lhs)

	}
	type PointOfFractional <: PointOfFractionalImplem
	val pointOfFractional: PointOfFractional
	type FractionalPointOps = pointOfFractional.FractionalPointOps
}

object FractionalStable {
	trait Of[T] {
		type For[Repr[X] <: Point[X]] = OfFor[T, Repr[T]]
		type PointOfFractional[Repr[X] <: Point[X]] = For[Repr]#PointOfFractional
	}
	type OfFor[T, _Repr <: Point[T]] = FractionalStable[T] {
		type Repr = _Repr
	}
}
