package com.github.point
package numeric.complex

import d2.complex.Point
import org.specs2.Specification

/**
	* Classic "import Numeric.Implicits._" usage: no scalar ops
	*/
class ClassicFractionalPointSpec extends Specification {def is = s2"""
	This is a specification for points as classic integrals

  The point should behave
    (1, 2) + (2, 3) = (3, 5)													$e1
    (1, 2) * (2, 3) = (2, 6)													$e2
    (2, 3) / (1, 2) = (2, 1)													$e3
																											"""
	import Fractional.Implicits._
	import Point.Implicits._
	import FractionalStable.pointOfFractional

	val x1 = Point(1.0, 2)
	val x2 = Point(2.0, 3)
	val e1 = (x1 + x2) ==== Point(3.0, 5)
	val e2 = (x1 * x2) ==== Point(2.0, 6)
	val e3 = (x2 / x1) ==== Point(2.0, 1.5)
	//val e4 = (x1 / x2) ==== Point(0.5, 1.5)
}
