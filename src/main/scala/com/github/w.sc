import scala.language.implicitConversions

class Toto[T](x: T) {
	def coucou = "coucou"
}

object Hold {
	trait H[T]

	object H {
		implicit def getH[Int] = new H[Int] {}
	}
}

import Hold.H

implicit def toto[T : H](x: T): Toto[T] = new Toto(x)

//import H._

1.coucou
