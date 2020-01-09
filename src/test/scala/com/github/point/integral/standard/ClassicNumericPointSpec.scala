package com.github.point.integral.standard

import org.specs2.Specification

/**
	* Classic "import Numeric.Implicits._" usage: no scalar ops
	*/
class ClassicNumericPointSpec extends Specification {def is = s2"""
	This is a specification for points as classic numerics

  The point should behave
    (1, 2) + (2, 3) = (3, 5)													$e1
    (1, 2) * (2, 3) = (2, 6)													$e2
																											"""

	val x1 = Point(1, 2)
	val x2 = Point(2, 3)
	val e1 = (x1 + x2) ==== Point(3, 5)
	val e2 = (x1 * x2) ==== Point(2, 6)
}
