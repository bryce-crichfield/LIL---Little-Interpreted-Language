import Token.TokenType

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

  sealed trait TokenType

  object TokenType {
    private val alpha = """([a-zA-Z])+""".r
    private val numeric = """[-]?(\d+[.]\d+|\d+)""".r
    private val string = """["]([a-zA-Z0-9])*["]""".r

    def apply(lexeme: String): TokenType = lexeme match {
      case "begin" => Begin
      case "end" => End
      case "program" => Program
      case "variables" => Variables
      case "define" => Define
      case "return" => Return
      case "does" => Does
      case "takes" => Takes
      case "procedure" => Procedure
      case "run" => Run
      case "function" => Function
      case "actions" => Actions
      case "display" => Display
      case "," => Comma
      case "set" => Set
      case "to" => To
      case "=" => Equals
      case "if" => If
      case "then" => Then
      case "else" => Else
      case "while" => While
      case "do" => Do
      case "||" => Or
      case "&&" => And
      case "(" => LParen
      case ")" => RParen
      case "==" => Equiv
      case "~=" => NEquiv
      case "+" => Plus
      case "-" => Minus
      case "*" => Multiply
      case "/" => Divide
      case "%" => Modulo
      case "->" => Dereference
      case "~" => Negate
      case "true" => True
      case "false" => False
      case "<" => LsThan
      case ">" => GrThan
      case "<=" => GrEqThan
      case ">=" => LsEqThan
      case "@" => Pointer
      case "\"" => Quote
      case "as" => As
      case string(_) => String
      case alpha(_) => Identifier
      case numeric(_) => Number
      case _ => TokenError
    }
  }

  case object Begin extends TokenType

  case object End extends TokenType

  case object Program extends TokenType

  case object Variables extends TokenType

  case object Define extends TokenType

  case object Procedure extends TokenType

  case object Do extends TokenType

  case object Function extends TokenType

  case object Return extends TokenType

  case object Does extends TokenType

  case object Takes extends TokenType

  case object Actions extends TokenType

  case object Identifier extends TokenType

  case object Display extends TokenType

  case object Pointer extends TokenType

  case object Comma extends TokenType

  case object Set extends TokenType

  case object To extends TokenType

  case object Equals extends TokenType

  case object If extends TokenType

  case object Then extends TokenType

  case object Else extends TokenType

  case object While extends TokenType

  case object Run extends TokenType

  case object Or extends TokenType

  case object And extends TokenType

  case object LParen extends TokenType

  case object RParen extends TokenType

  case object Equiv extends TokenType

  case object NEquiv extends TokenType

  case object Plus extends TokenType

  case object Minus extends TokenType

  case object Multiply extends TokenType

  case object Divide extends TokenType

  case object Modulo extends TokenType

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

  case object TokenError extends TokenType

}

