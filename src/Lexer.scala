

object Lexer {
  private val regex = """->|==|<=|>=|<|>|=|&&|[|]{2}|~|,|@|%|[+]|[*]|/|[(]|[)]|["]([a-zA-Z])*["]|([a-zA-Z0-9])+|[-]?(\d+[.]\d+|\d+)|[-]""".r

  //  private val regex = """(\w)+""".r
  def apply(input: String): List[Token] = {
    val lines = input.split("\n")
    lines.flatMap(l => processLine(l, lines.indexOf(l))).toList
  }

  private def processLine(line: String, number: Int): List[Token] = {
    val matches = for (m <- regex.findAllIn(line)) yield m.mkString
    matches.map(m => Token(m, number)).toList
  }

}
