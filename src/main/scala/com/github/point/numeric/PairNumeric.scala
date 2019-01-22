package com.github.point.numeric

import language.higherKinds

/**
	* Created by Nicolas on 01/06/2017.
	*/

class PairNumeric[T : Numeric, U : Numeric] extends PairTemplate[Numeric, T, U] with Numeric[(T, U)]

abstract class PairTemplate[M[X] <: Numeric[X], T : M, U : M] {
	trait BinaryOp {
		def op[X]: M[X] => (X, X) => X
		def apply[X : M](x: X, y: X): X = op(implicitly[M[X]])(x, y)
	}
	class BinaryOpTemplate {
		val plus = new BinaryOp {def op[X] = _.plus}
		val minus = new BinaryOp {def op[X] = _.minus}
		val times = new BinaryOp {def op[X] = _.times}
	}
	object BinaryOp extends BinaryOpTemplate

	protected def lift(op: BinaryOp)(x: (T, U), y: (T, U)) =
		(op(x._1, y._1),op(x._2, y._2))

	def plus(x: (T, U), y: (T, U)): (T, U) = lift(BinaryOp.plus)(x, y)
	def minus(x: (T, U), y: (T, U)): (T, U) = lift(BinaryOp.minus)(x, y)
	def times(x: (T, U), y: (T, U)): (T, U) = lift(BinaryOp.times)(x, y)
	import Numeric.Implicits._
	def negate(x: (T, U)): (T, U) = (-x._1, -x._2)
	def fromInt(x: Int): (T, U) = (implicitly[Numeric[T]].fromInt(x), implicitly[Numeric[U]].fromInt(0))
	def parseString(str: String): Option[(T, U)] = None
	def toInt(x: (T, U)): Int = x._1.toInt
	def toLong(x: (T, U)): Long = x._1.toLong
	def toFloat(x: (T, U)): Float = x._1.toFloat
	def toDouble(x: (T, U)): Double = x._1.toDouble
	def compare(x: (T, U), y: (T, U)): Int = {
		import Ordering.Implicits._
		if (x._1 < y._1) -1
		else if (x._1 > y._1) 1
		else if (x._2 < y._2) -1
		else if (x._2 > y._2) 1
		else 0
	}
}
