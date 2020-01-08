package com.github.point
package numeric
package complex

import org.specs2._
import d2.complex.Point

/**
	* Classic "import Numeric.Implicits._" usage: no scalar ops
	*/
class ClassicNumericPointSpec extends Specification {def is = s2"""
	This is a specification for points as classic numerics

  The point should behave
    (1 + 2i) + (2 + 3i) = 3 + 5i											$e1
    (1 + 2i) * (2 + 3i) = -4 + 7i											$e2
																											"""
	import Numeric.Implicits._
	import Point.Implicits._
	import NumericStable.pointOfNumeric

	val x1 = Point(1, 2)
	val x2 = Point(2, 3)
	val e1 = (x1 + x2) ==== Point(3, 5)
	val e2 = (x1 * x2) ==== Point(-4, 7)
}
