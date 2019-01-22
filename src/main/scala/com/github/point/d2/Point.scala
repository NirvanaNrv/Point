package com.github
package point
package d2

/**
	* Created by Nicolas on 28/03/2017.
	*/
trait Point[T] extends point.Point.Point2[T] {
	val companion: Point.Companion
	val x: T
	val y: T
	val (_1, _2) = (x, y)
}

object Point extends HigherKinded {
	trait Companion extends point.Point.Point2.Companion {type Bound[T] <: Point[T]}
}
