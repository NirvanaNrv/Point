package com.github
package point.numeric

import point.d2.standard.Point
import org.specs2.Specification

/**
	* Extended numeric ops: replaces "import Numeric.Implicits._", and gets scalar ops
	*/
class ScalarNumericPointSpec extends Specification {def is = s2"""
	This is a specification for points as scalar numerics

  The point should behave
    (1, 2) + (2, 3) = (3, 5)													$e1
    (1, 2) * 2 = (2, 4)																$e2
																											"""

	//import Numeric.Implicits._
	//That one is already included in the imported extended ops which carries the scalar operations too
	import Point.Implicits._
	import Implicits.Scalar.numeric._

	val x1 = Point(1, 2)
	val x2 = Point(2, 3)

	val e1 = (x1 + x2) ==== Point(3, 5)
	val e2 = x1 * 2 ==== Point(2, 4)
}