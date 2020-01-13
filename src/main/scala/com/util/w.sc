import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.matching.Regex.{Groups, Match}

val data = """\\"hallo "\\\\\\"lo wo\"rld" \\\""here "it is" not there"""

def tokenCharGroup(exclude: String) = s"""(?:\\.|[^"$exclude])"""
val inQuote = s"${tokenCharGroup("")}*+"
val outQuote = s"${tokenCharGroup(" ")}++"

val regex = {
	s"""(?:"($inQuote)"|($outQuote))++""".r
}

def display(regex: Regex) =
	regex.findAllMatchIn(data).map(
		m => m -> m.subgroups.mkString("!")
	).mkString("|")

display(regex)

regex.matches()

display(s""""(?:(\\\\)|(a)|[^"])*+"""".r)
