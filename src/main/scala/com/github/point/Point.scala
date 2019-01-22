package com.github
package point

import language.higherKinds

/**
	* Wondering where to find that elsewhere; homogeneous tuples... not quite HList, not quite Seq, nor quite tuples
	*/
trait Point[T] extends Product {
	val companion: Point.Companion
	type Companion = companion.type
	import companion.{Bound, Repr}

	val _1: T
	abstract override def productElement(n: Int): T = super.productElement(n).asInstanceOf[T]
	def canEqual(that: Any): Boolean = that match {
		case p: Point[T] => productArity == p.productArity
		case _ => false
	}
	override def productIterator: Iterator[T] = super.productIterator.asInstanceOf[Iterator[T]]
	def toList: List[T] = productIterator.toList
	def fromSeq(seq: Seq[T]): Repr[T]
	def updated(i: Int, value: T) = fromSeq(toList.updated(i, value))
	def apply(i: Int) = productElement(i)
	def map[U](f: T => U): Bound[U] = companion.fromSeq(toList.map(f))
	def reverse = fromSeq(toList.reverse)
	def zip[U](point: Bound[U]): Bound[(T, U)] = companion.fromSeq(toList.zip(point.toList))
	override def toString: String = s"(${toList.mkString(",")})"
}

object Point {
	trait Companion {
		type Bound[T] <: Point[T]
		type Repr[T] <: Bound[T]
		def fromSeq[T](seq: Seq[T]): Bound[T]
	}
	sealed trait PointGt2[T] extends Point[T] {
		val _2: T
	}

	trait Point1[T] extends Product1[T] with Point[T] {val companion: Point1.Companion}
	trait Point2[T] extends Product2[T, T] with PointGt2[T] {
		val companion: Point2.Companion
		def swap = reverse
	}
	trait Point3[T] extends Product3[T, T, T] with PointGt2[T] {val companion: Point3.Companion}
	trait Point4[T] extends Product4[T, T, T, T] with PointGt2[T] {val companion: Point4.Companion}
	trait Point5[T] extends Product5[T, T, T, T, T] with PointGt2[T] {val companion: Point5.Companion}
	trait Point6[T] extends Product6[T, T, T, T, T, T] with PointGt2[T] {val companion: Point6.Companion}
	trait Point7[T] extends Product7[T, T, T, T, T, T, T] with PointGt2[T] {val companion: Point7.Companion}

	object Point1 {trait Companion extends Point.Companion {type Bound[X] <: Point1[X]}}
	object Point2 {trait Companion extends Point.Companion {type Bound[X] <: Point2[X]}}
	object Point3 {trait Companion extends Point.Companion {type Bound[X] <: Point3[X]}}
	object Point4 {trait Companion extends Point.Companion {type Bound[X] <: Point4[X]}}
	object Point5 {trait Companion extends Point.Companion {type Bound[X] <: Point5[X]}}
	object Point6 {trait Companion extends Point.Companion {type Bound[X] <: Point6[X]}}
	object Point7 {trait Companion extends Point.Companion {type Bound[X] <: Point7[X]}}
}
