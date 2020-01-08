case class Toto(x: Int) {
	def unary_~ = x
}
implicit class Op(x: Toto) {
	def toto = x
	def ! = x
}

val x = Toto(0)
x.!
x!
