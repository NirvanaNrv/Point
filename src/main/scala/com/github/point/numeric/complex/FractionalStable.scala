package com.github
package point
package numeric
package complex

/**
	* Created by Nicolas on 24/03/2017.
	*/
abstract class FractionalStable[T : Fractional] extends numeric.FractionalStable[T] {
	class PointOfFractionalImplem extends super.PointOfFractionalImplem {
		type FractionalPointOps = FractionalPointOpsImplem
		def mkFractionalOpsImplem(lhs: Repr) = new FractionalPointOpsImplem(lhs)
	}
	type PointOfFractional = PointOfFractionalImplem
	val pointOfFractional = new PointOfFractionalImplem
}

object FractionalStable {
	trait Of[T] {
		type For[Repr[X] <: Point[X]] = OfFor[T, Repr[T]]
		type PointOfFractional[Repr[X] <: Point[X]] = For[Repr]#PointOfFractional
	}
	type OfFor[T, _Repr <: Point[T]] = FractionalStable[T] {
		type Repr = _Repr
	}

	implicit def stableToFractionalStable[T: Fractional, Repr[X] <: Point[X] : StableCompanion.For]: OfFor[T, Repr[T]] = {
		val companion = implicitly[StableCompanion.For[Repr]]
		type ReprOfT = Repr[T]
		new FractionalStable[T] {
			type Repr = ReprOfT
			def fromSeq(seq: Seq[T]): Repr = companion.fromSeq(seq)
		}
	}

	implicit def pointOfFractional[T : Integral, Repr[X] <: point.Point[X] : Of[T]#For]: OfFor[T, Repr[T]]#PointOfFractional = {
		val n = implicitly[OfFor[T, Repr[T]]]
		new n.PointOfFractional
	}

	implicit def pointOfFractionalOps[T, Repr[X] <: point.Point[X] : Of[T]#For](x: Repr[T]): OfFor[T, Repr[T]]#FractionalPointOps = {
		val fractionalStable = implicitly[OfFor[T, Repr[T]]]
		new fractionalStable.pointOfFractional.FractionalPointOps(x)
	}
}
