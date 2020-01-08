package com.github
package point

trait StableCompanion extends Point.Companion {companion =>
	type Repr[T] = Bound[T]
}

object StableCompanion {
	type For[_Repr[X] <: Point[X]] = StableCompanion {type Bound[T] = _Repr[T]}
}