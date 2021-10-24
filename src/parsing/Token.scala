package parsing

case class Token(tokenType: TokenType, lexeme: String, lineNumber: Int) {

}

object Token {
  def apply(lexeme: String, number: Int): Token = {
    val tokenType = TokenType(lexeme)
    tokenType match {
      case String =>
        val content = lexeme.tail.reverse.tail.reverse
        Token(tokenType, content, number)
      case _ => Token(tokenType, lexeme, number)
    }
  }
}

sealed trait TokenType

object TokenType {
  def apply(input: String): TokenType = {
    val alpha = """([a-zA-Z])+""".r
    val numeric = """[-]?(\d+[.]\d+|\d+)""".r
    val string = """["]([a-zA-Z0-9])*["]""".r
    input match {
      case "variables" => (Variables)
      case "define" => (Define)
      case "actions" => Actions
      case "endProgram" => EndProgram
      case "display" => (Display)
      case "," => (Comma)
      case "set" => (Set)
      case "=" => (Equals)
      case "if" => (If)
      case "then" => (Then)
      case "else" => (Else)
      case "endif" => (EndIf)
      case "while" => (While)
      case "do" => (Do)
      case "endwhile" => (EndWhile)
      case "||" => (Or)
      case "&&" => (And)
      case "(" => (LParen)
      case ")" => (RParen)
      case "==" => (Equiv)
      case "+" => (Plus)
      case "-" => (Minus)
      case "*" => (Multiply)
      case "/" => (Divide)
      case "%" => (Modulo)
      case "->" => (Dereference)
      case "~" => (Negate)
      case "true" => (True)
      case "false" => (False)
      case "<" => (LsThan)
      case ">" => (GrThan)
      case "<=" => (GrEqThan)
      case ">=" => (LsEqThan)
      case "@" => (Pointer)
      case "\"" => (Quote)
      case "as" => (As)
      case string(_) => String
      case alpha(_) => Identifier
      case numeric(_) => Number
      case _ => TokenError
    }
  }
}

case object Variables extends TokenType

case object Define extends TokenType

case object Actions extends TokenType

case object Identifier extends TokenType

case object Display extends TokenType

case object Pointer extends TokenType

case object Comma extends TokenType

case object Set extends TokenType

case object Equals extends TokenType

case object If extends TokenType

case object Then extends TokenType

case object Else extends TokenType

case object EndIf extends TokenType

case object While extends TokenType

case object Do extends TokenType

case object EndWhile extends TokenType

case object Or extends TokenType

case object And extends TokenType

case object LParen extends TokenType

case object RParen extends TokenType

case object Equiv extends TokenType

case object Plus extends TokenType

case object Minus extends TokenType

case object Multiply extends TokenType with TermOperator

case object Divide extends TokenType with TermOperator

case object Modulo extends TokenType with TermOperator

case object Dereference extends TokenType

case object Negate extends TokenType

case object Quote extends TokenType

case object True extends TokenType

case object False extends TokenType

case object LsThan extends TokenType

case object GrThan extends TokenType

case object LsEqThan extends TokenType

case object GrEqThan extends TokenType

case object Number extends TokenType

case object String extends TokenType

case object As extends TokenType

case object EOF extends TokenType
case object EndProgram extends TokenType

case object TokenError extends TokenType


trait TArgument
sealed trait TermOperator
