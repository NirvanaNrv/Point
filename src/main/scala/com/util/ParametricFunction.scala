package com.util

import language.higherKinds

/**
	* Created by Nicolas on 26/03/2017.
	*/
trait ParametricFunction[-Source[T], +Target[T]] {
	def apply[T](source: Source[T]): Target[T]
}
