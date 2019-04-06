package com.github
package point
package numeric

/**
	* Created by Nicolas on 24/03/2017.
	*/
abstract class IntegralStable[T : Integral] extends Stable[Integral, T] {
	protected val isIntegral: Integral[T] = implicitly[Integral[T]]

	trait IntegralPoint extends NumericPoint {
		def quot(x: Repr, y: Repr): Repr = opLift(_.quot)(x, y)
		def rem(x: Repr, y: Repr): Repr = opLift(_.rem)(x, y)
		def scalarQuot(x: Repr, y: T): Repr = scalarLift(_.quot)(x, y)
		def scalarRem(x: Repr, y: T): Repr = scalarLift(_.rem)(x, y)

		trait IntegralPointScalarOps extends NumericPointScalarOps {
			def /(rhs: T): Repr = scalarQuot(lhs, rhs)
			def %(rhs: T): Repr = scalarRem(lhs, rhs)
		}
	}

	class PointOfIntegral extends Integral[Repr] with IntegralPoint {
		class IntegralPointOps(protected val lhs: Repr) extends NumericOps(lhs) with IntegralPointScalarOps
	}
	object PointOfIntegral {
		def pointOfIntegral[T : Integral, Repr[X] <: point.Point[X] : IntegralStable.Of[T]#For]: IntegralStable.OfFor[T, Repr[T]]#PointOfIntegral = {
			val n = implicitly[IntegralStable.OfFor[T, Repr[T]]]
			new n.PointOfIntegral
		}
	}
}

object IntegralStable extends HigherKinded {
	trait Of[T] {
		type For[Repr[X] <: Point[X]] = OfFor[T, Repr[T]]
		type PointOfIntegral[Repr[X] <: Point[X]] = For[Repr]#PointOfIntegral
	}
	type OfFor[T, _Repr <: Point[T]] = IntegralStable[T] {
		type Repr = _Repr
	}
}
