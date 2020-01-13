import scala.annotation.tailrec
import scala.util.matching.Regex.{Groups, Match}

implicit class OptionExtension[T](option: Option[T]) {
	def or(that: T) = option.orElse(Some(that))
}

@tailrec
def parse(line: List[Char], result: List[String], token: Option[List[Char]], quoted: Boolean, escaped: Boolean = false): Seq[String] = {
	def addToken = token.map(_.reverse.mkString).toList ::: result
	line match {
		case char :: line =>
			def addChar = token.or(Nil).map(char :: _)
			char match {
				case ' ' if !quoted => parse(line, addToken, None, false)
				case '\\' if !escaped => parse(line, result, token, quoted, true)
				case '"' if !escaped => parse(line, result, token, !quoted)
				case _ => parse(line, result, addChar, quoted)
			}
		case _ => addToken.reverse
	}
}
def parseLineTailRec(line: String): Seq[String] = parse(line.toList, Nil, None, false)
val regex = {
	def tokenCharGroup(exclude: String) = s"""(?:\\.|[^"$exclude])"""
	val inQuote = s"${tokenCharGroup("")}*+"
	val outQuote = s"${tokenCharGroup(" ")}++"
	s"""(?:"($inQuote)"|($outQuote))++""".r
}
val replaceBackslash = """\\(.)""".r
def parseLineRegEx(line: String): Seq[String] = {
	def replacer: Match => String = {case Groups(s) => s"\\$s"}
	regex.findAllMatchIn(line)
		.map(_.subgroups.flatMap(Option(_).toList).mkString)
		//.map(replaceBackslash.replaceAllIn(_, replacer))
  	.toSeq
}
def parseLine(line: String): Seq[String] = parseLineTailRec(line)

val data = """\\"hallo "\\\\\\"lo wo\"rld" \\\""here "it is" not there"""
parseLineTailRec(data).mkString("|")
parseLineRegEx(data).mkString("|")
