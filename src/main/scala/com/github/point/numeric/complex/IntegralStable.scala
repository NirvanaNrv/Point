package com.github
package point
package numeric
package complex

import scala.language.implicitConversions

/**
	* Created by Nicolas on 24/03/2017.
	*/
abstract class IntegralStable[T : Integral] extends numeric.IntegralStable[T] {
	class PointOfIntegralImplem extends super.PointOfIntegralImplem {
		type IntegralPointOps = IntegralPointOpsImplem
		def mkIntegralOpsImplem(lhs: Repr) = new IntegralPointOpsImplem(lhs)
	}
	type PointOfIntegral = PointOfIntegralImplem
	val pointOfIntegral = new PointOfIntegralImplem
}

object IntegralStable {
	trait Of[T] {
		type For[Repr[X] <: Point[X]] = OfFor[T, Repr[T]]
		type PointOfIntegral[Repr[X] <: Point[X]] = For[Repr]#PointOfIntegral
	}
	type OfFor[T, _Repr <: Point[T]] = IntegralStable[T] {
		type Repr = _Repr
	}

	implicit def stableToIntegralStable[T: Integral, Repr[X] <: Point[X] : StableCompanion.For]: OfFor[T, Repr[T]] = {
		val companion = implicitly[StableCompanion.For[Repr]]
		type ReprOfT = Repr[T]
		new IntegralStable[T] {
			type Repr = ReprOfT
			def fromSeq(seq: Seq[T]): Repr = companion.fromSeq(seq)
		}
	}

	implicit def pointOfIntegral[T : Integral, Repr[X] <: point.Point[X] : Of[T]#For]: OfFor[T, Repr[T]]#PointOfIntegral = {
		val n = implicitly[OfFor[T, Repr[T]]]
		new n.PointOfIntegral
	}

	implicit def pointOfIntegralOps[T, Repr[X] <: point.Point[X] : Of[T]#For](x: Repr[T]): OfFor[T, Repr[T]]#IntegralPointOps = {
		val integralStable = implicitly[OfFor[T, Repr[T]]]
		new integralStable.pointOfIntegral.IntegralPointOps(x)
	}
}
