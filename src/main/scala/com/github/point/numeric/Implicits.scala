package com.github
package point
package numeric

/**
	* Created by Nicolas on 24/03/2017.
	*/
trait LowPriorityStableImplicits extends point.Implicits with HigherKinded {
	/*implicit def stableToNumericStable[T : Numeric, Repr[X] <: Point[X] : StableCompanion.For]: NumericStable.Of[T]#For[Repr] = {
		val companion = implicitly[StableCompanion.For[Repr]]
		type ReprOfT = Repr[T]
		new NumericStable[T] {
			type Repr = ReprOfT
			def fromSeq(seq: Seq[T]): Repr = companion.fromSeq(seq)
		}
	}
	*/
}

trait LowPriorityClassicImplicits extends LowPriorityStableImplicits {
	implicit def pointOfNumeric[T, Repr[X] <: point.Point[X] : NumericStable.Of[T]#For]: NumericStable.Of[T]#For[Repr]#PointOfNumeric = {
		val n = implicitly[NumericStable.OfFor[T, Repr[T]]]
		new n.PointOfNumeric
	}
}

trait LowPriorityImplicits extends LowPriorityClassicImplicits {
	//As I tried to use a bottom order Repr <: d.Point[T], I got inferred T of "Nothing" for Point[Int], instead of Int
	// -> use a higher order ensuring a trivial match for the compiler from the x parameter type
	implicit def pointOfNumericOps[T, Repr[X] <: point.Point[X] : NumericStable.Of[T]#PointOfNumeric](x: Repr[T]): NumericStable.OfFor[T, Repr[T]]#PointOfNumeric#NumericPointOps = {
		val num = implicitly[NumericStable.OfFor[T, Repr[T]]#PointOfNumeric]
		new num.NumericPointOps(x)
	}
}

trait HighPriorityStableImplicits extends LowPriorityStableImplicits {
	implicit def stableToIntegralStable[T: Integral, Repr[X] <: Point[X] : StableCompanion.For]: IntegralStable.OfFor[T, Repr[T]] = {
		val companion = implicitly[StableCompanion.For[Repr]]
		type ReprOfT = Repr[T]
		new IntegralStable[T] {
			type Repr = ReprOfT
			def fromSeq(seq: Seq[T]): Repr = companion.fromSeq(seq)
		}
	}
	implicit def stableToFractionalStable[T: Fractional, Repr[X] <: Point[X] : StableCompanion.For]: FractionalStable.OfFor[T, Repr[T]] = {
		val companion = implicitly[StableCompanion.For[Repr]]
		type ReprOfT = Repr[T]
		new FractionalStable[T] {
			type Repr = ReprOfT
			def fromSeq(seq: Seq[T]): Repr = companion.fromSeq(seq)
		}
	}
}

trait HighPriorityClassicImplicits extends LowPriorityClassicImplicits with HighPriorityStableImplicits {
	implicit def pointOfIntegral[T : Integral, Repr[X] <: point.Point[X] : IntegralStable.Of[T]#For]: IntegralStable.OfFor[T, Repr[T]]#PointOfIntegral = {
		val n = implicitly[IntegralStable.OfFor[T, Repr[T]]]
		new n.PointOfIntegral
	}
	implicit def pointOfFractional[T : Fractional, Repr[X] <: point.Point[X] : FractionalStable.Of[T]#For]: FractionalStable.OfFor[T, Repr[T]]#PointOfFractional = {
		val n = implicitly[FractionalStable.OfFor[T, Repr[T]]]
		new n.PointOfFractional
	}
}

trait HighPriorityImplicits extends HighPriorityClassicImplicits with LowPriorityImplicits {
	implicit def pointOfIntegralOps[T, Repr[X] <: point.Point[X] : IntegralStable.Of[T]#PointOfIntegral](x: Repr[T]): IntegralStable.OfFor[T, Repr[T]]#PointOfIntegral#IntegralPointOps = {
		val num = implicitly[IntegralStable.OfFor[T, Repr[T]]#PointOfIntegral]
		new num.IntegralPointOps(x)
	}
	implicit def pointOfFractionalOps[T, Repr[X] <: point.Point[X] : FractionalStable.Of[T]#PointOfFractional](x: Repr[T]): FractionalStable.OfFor[T, Repr[T]]#PointOfFractional#FractionalPointOps = {
		val num = implicitly[FractionalStable.OfFor[T, Repr[T]]#PointOfFractional]
		new num.FractionalPointOps(x)
	}
}

object Implicits extends HighPriorityImplicits {implicits =>
	object Classic {
		val numeric: LowPriorityClassicImplicits = implicits
		val all: HighPriorityClassicImplicits =  implicits
	}
	object Scalar {
		val numeric: LowPriorityImplicits = implicits
		val all: HighPriorityImplicits =  implicits
	}
}
