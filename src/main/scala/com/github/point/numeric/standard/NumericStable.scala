package com.github
package point
package numeric
package standard

import scala.language.{existentials, implicitConversions, reflectiveCalls}

/**
	* Created by Nicolas on 24/03/2017.
	*/
abstract class NumericStable[T : Numeric] extends numeric.NumericStable[T] {
	class PointOfNumericImplem extends super.PointOfNumericImplem {
		type NumericPointOps = NumericPointOpsImplem
		def mkNumericOpsImplem(lhs: Repr) = new NumericPointOpsImplem(lhs)
	}
	type PointOfNumeric = PointOfNumericImplem
	val pointOfNumeric = new PointOfNumericImplem
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

	implicit def stableToNumericStable[T : Numeric, Repr[X] <: Point[X] : StableCompanion.For]: Of[T]#For[Repr] = {
		val companion = implicitly[StableCompanion.For[Repr]]
		type ReprOfT = Repr[T]
		new NumericStable[T] {
			type Repr = ReprOfT
			def fromSeq(seq: Seq[T]): Repr = companion.fromSeq(seq)
		}
	}

	implicit def pointOfNumeric[T, Repr[X] <: Point[X] : Of[T]#For]: Of[T]#For[Repr]#PointOfNumeric = {
		val n = implicitly[OfFor[T, Repr[T]]]
		n.pointOfNumeric
	}

	//As I tried to use a bottom order Repr <: d.Point[T], I got inferred T of "Nothing" for Point[Int], instead of Int
	// -> use a higher order ensuring a trivial match for the compiler from the x parameter type
	implicit def pointOfNumericOps[T, Repr[X] <: Point[X] : Of[T]#For](x: Repr[T]): OfFor[T, Repr[T]]#NumericPointOps = {
		val numericStable = implicitly[OfFor[T, Repr[T]]]
		numericStable.pointOfNumeric.mkNumericOpsImplem(x)
	}
}
