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
    (1, 2) + (2, 3) = (3, 5)													$e1
																											"""

	import Point.Implicits._
	import Implicits.Scalar.numeric._

	val x1 = Point(1, 2)
	val x2 = Point(2, 3)

	val e1 = (x1 + x2) ==== Point(3, 5)
	val e2 = x1 * 2 ==== Point(2, 4)
}
