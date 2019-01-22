package com.github
package point.numeric

import point.d2.standard.Point
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

	import Numeric.Implicits._
	import Point.Implicits._
	import Implicits.Classic.numeric._

	val x1 = Point(1, 2)
	val x2 = Point(2, 3)
	val e1 = (x1 + x2) ==== Point(3, 5)
	val e2 = (x1 * x2) ==== Point(2, 6)
}
