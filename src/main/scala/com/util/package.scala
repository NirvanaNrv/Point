package com

/**
	* Created by Nicolas on 04/03/2017.
	*/
package object util {
	def sign(positive: Boolean): Int = if (positive) 1 else -1

	def timed[T](f: => T): T = {
		val start = System.nanoTime
		val result = f
		val lasted = (System.nanoTime - start) / 1000000
		println(s"Time: $lasted ms")
		result
	}

	/**
		* This is simple inversion between a function and its argument.
		* It is helpful to 'recycle' (shadow) a value name which we would like to transform.
		*
		* @param x		the parameter of the unary function f
		* @param f		the unary function to apply to x immediately
		* @tparam T		the parameter type
		* @tparam U		the function return type
		* @return			returns f(x)
		*
		* 	Example:
		* 	{{{
		*			val x = 5
		*			using (x + 1) {x =>
		*				println(x)
		*			}
		* 	}}}
		*
		*/
	def using[T, U](x: T)(f: T => U) = f(x)
}
