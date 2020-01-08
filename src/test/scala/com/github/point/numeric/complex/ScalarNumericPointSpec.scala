package com.github
package point
package numeric
package complex

import org.specs2._
import d2.complex.Point

/**
	* Extended numeric ops: replaces "import Numeric.Implicits._", and gets scalar ops
	*/
class ScalarNumericPointSpec extends Specification {def is = s2"""
	This is a specification for points as scalar numerics

  The point should behave
    (1 + 2i) + (2 + 3i) = 3 + 5i											$e1
    (1 + 2i) * 2 = 2 + 4i															$e2
    (1 + 2i) * i = -2 + i															$e3
    !(1 + 2i) = 1 - 2i																$e4
    !(1 + 2i)! = 1 + 2i																$e5
																											"""

	import Point.Implicits._
	import NumericStable.pointOfNumericOps

	val x1 = Point(1, 2)
	val x2 = Point(2, 3)

	val e1 = (x1 + x2) ==== Point(3, 5)
	val e2 = x1 * 2 ==== Point(2, 4)
	val e3 = x1.i ==== Point(-2, 1)
	val e4 = !x1 ==== Point(1, -2)
	val e5 = (!x1!) ==== x1
}
