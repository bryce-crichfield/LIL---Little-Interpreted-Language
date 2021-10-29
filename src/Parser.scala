import Model._
import Token.{Procedure, Program, _}

object Parser {

  // A Parsed of T in an ADT that represents that result of a parsing computation on type T
  // A parsing computation wraps a type that it aims to parse such as Parsed[Statement]
  // If the computation fails to parse that type (ie the input does not conform to the grammar) then
  // the computation will return a failure on that type.
  // However, if the computation is successful, it will return a success which wraps all the found instances of that type
  // plus the remaining tokens that were captured by the parsing computation
  sealed trait Parsed[A] {
    val targets: List[A]
    val tokens: List[Token]

    def |>[B](f: (A, List[Token]) => (B, List[Token])): Parsed[B] = {
      this match {
        case Success(targets, tokens) =>
          val (b, tkns) = f(targets.head, tokens)
          Success(List(b), tkns)
        case Failure(msg) => Failure(msg)
      }
    }

    def +|>[B](f: (List[A], List[Token]) => (B, List[Token])): Parsed[B] = {
      this match {
        case Success(targets, tokens) =>
          val (b, t1) = f(targets, tokens)
          Success(b, t1)
        case Failure(msg) => Failure(msg)
      }
    }

    // takes two functions to handle the case of a success and the case of a null value
    def ?|>[B](s: (A, List[Token]) => (B, List[Token]))(n: List[Token] => (B, List[Token]))
    : Parsed[B] = {
      this match {
        case Success(targets, tokens) =>
          val (b1, tkns) = s(targets.head, tokens)
          Success(List(b1), tkns)
        case Null(tokens) =>
          val (b2, tkns) = n(tokens)
          Success(List(b2), tkns)
        case Failure(msg) => Failure(msg)
      }
    }

    // allows the chaining of advance() operations
    def ||>[B](f: (A, List[Token]) => Parsed[B]): Parsed[B] = {
      this match {
        case Success(targets, tokens) if targets.nonEmpty =>
          f(targets.head, tokens)
        case Failure(msg) => Failure(msg)
      }
    }

    // FIXME: This is causing issue with parsing if statement
    def +||>[B](f: (List[A], List[Token]) => Parsed[B]): Parsed[B] = {
      this match {
        case Success(targets, tokens) =>
          f(targets, tokens)
        case Failure(msg) => Failure(msg)
      }
    }

    def ?||>[B](s: (A, List[Token]) => Parsed[B])(n: List[Token] => Parsed[B]): Parsed[B] = {
      this match {
        case Success(targets, tokens) =>
          s(targets.head, tokens)
        case Null(tokens) =>
          n(tokens)
        case Failure(msg) => Failure(msg)
      }
    }

    def *||>[B](s: (List[A], List[Token]) => Parsed[B])(n: List[Token] => Parsed[B]): Parsed[B] = {
      this match {
        case Success(targets, tokens) =>
          s(targets, tokens)
        case Null(tokens) =>
          n(tokens)
        case Failure(msg) => Failure(msg)
      }
    }

  }

  case class Success[A](targets: List[A], tokens: List[Token]) extends Parsed[A]

  object Success {
    def apply[A](t: A, tokens: List[Token]): Success[A] = {
      Success(List(t), tokens)
    }
  }

  case class Failure[A](msg: String) extends Parsed[A] {
    val targets: List[A] = List.empty[A]
    val tokens: List[Token] = List.empty[Token]
  }

  case class Null[A](tokens: List[Token]) extends Parsed[A] {
    val targets: List[A] = List.empty[A]
  }

  def parse(ts: List[Token]): Parsed[PROGRAM] = {
    ts.typeOfFirst match {
      case Program => parseStatements(ts.tail) match {
        case Success(actions, tokens) => Success(PROGRAM(actions), tokens)
        case Failure(msg) => Failure(msg)
      }
      case _ => Failure(errorMessage("program")(token = ts.head, "program or variables"))
    }
  }

  private def parseVariableDeclaration(ts: List[Token]): Parsed[VariableDeclaration] = {
    ts.typeOfFirst(2) match {
      case List(Identifier, As) =>
        val id = IDENTIFIER(ts.head.lexeme)
        println(id)
        parseExpression(ts.drop(2)) |> { (expression, t1) =>
          VariableDeclaration(id, expression) -> t1
        }
      case _ => Failure(errorMessage("variable declaration")(token = ts.head, "define, id, as, or actions"))
    }
  }

  private def parseStatements(tokens: List[Token], actions: List[Statement] = List.empty)
  : Parsed[Statement] = {
    // attempt to parse as expression, then statement
    parseExpression(tokens) match {
      case Success(expression, ts) =>
        parseStatements(ts, actions :+ expression.head.asInstanceOf[Statement])
      case _ => tokens.head.tokenType match {
        case Define =>
          parseVariableDeclaration(tokens.tail) ||> { (declaration, ts) =>
            parseStatements(ts, actions :+ declaration)
          }
        case Set =>
          parseAssignment(tokens.tail) ||> { (stmt, ts) =>
            parseStatements(ts, actions :+ stmt)
          }
        case While =>
          parseWhile(tokens.tail) ||> { (stmt, ts) =>
            parseStatements(ts, actions :+ stmt)
          }
        case If =>
          parseIf(tokens.tail) ||> { (stmt, ts) =>
            parseStatements(ts, actions :+ stmt)
          }
        case Display =>
          parseDisplay(tokens.tail) ||> { (stmt, ts) =>
            parseStatements(ts, actions :+ stmt)
          }
        case Procedure =>
          parseProcedure(tokens.tail) ||> { (stmt, ts) =>
            parseStatements(ts, actions :+ stmt)
          }
        case End =>
          Success(actions.filterNot(_.isInstanceOf[Expression]), tokens.tail) // the filter avoids weird issues with useless expressions being allowed to sit around
        case _ => Failure(errorMessage("statement")(tokens.head, "define, set, while, if, display, or end"))
      }
    }

  }

  private def parseAssignment(tokens: List[Token]): Parsed[AssignmentStatement] = {
    tokens.typeOfFirst(2) match {
      case List(Identifier, To) =>
        parseExpression(tokens.drop(2)) |> { (expression, t1) =>
          val id = IDENTIFIER(tokens.head.lexeme)
          AssignmentStatement(id, expression) -> t1
        }
      case _ => Failure(errorMessage("assignment")(tokens.head, "identifier or to"))
    }
  }

  private def parseWhile(ts: List[Token]): Parsed[WhileStatement] = {
    parseExpression(ts) ||> { (cond, t2) =>
      t2.typeOfFirst match {
        case Do => parseStatements(t2.tail) match { // this actually can capture nested while loops, because if an internal while loop is found, it will consume its own endWhileToken
          case Success(actions, t3) => Success(WhileStatement(cond, actions), t3)
          case Failure(msg) => Failure(msg)
        }
        case _ => Failure(errorMessage("while")(ts.head, "do"))
      }
    }
  }

  private def parseIf(ts: List[Token]): Parsed[IfStatement] = {
    parseExpression(ts) ||> { (condition, t1) =>
      t1.typeOfFirst match {
        case Then =>
          parseStatements(t1.tail) +||> { (actions, t2) =>
            parseElseIf(t2).*||> { (elifs, t3a) => // in the case of one or many elifs
              parseElse(t3a).?|> { (els, t4a) => // in the case of some elifs and some els
                IfStatement(condition, actions, Some(elifs), Some(els)) -> t4a
              } { t4b => // in the case of some elifs and some els
                IfStatement(condition, actions, Some(elifs), None) -> t4b
              }
            } { t3b => // in the case of no elifs
              parseElse(t3b).?|> { (els, t4a) => // in the case of no elifs and some els
                IfStatement(condition, actions, None, Some(els)) -> t4a
              } { t4b => // in the case of no elifs and no els
                IfStatement(condition, actions, None, None) -> t4b
              }
            }
          }
        case _ => Failure(errorMessage("if statement")(t1.head, "then"))
      }
    }
  }

  private def parseElseIf(ts: List[Token]): Parsed[ElseIfStatement] = {
    ts.typeOfFirst(2) match {
      case List(Else, If) => parseExpression(ts.drop(2)) ||> { (conditional, t1) =>
        parseStatements(t1) +|> { (actions, t2) =>
          ElseIfStatement(conditional, actions) -> t2
        }
      }
      case _ => Null(ts)
    }
  }

  private def parseElse(ts: List[Token]): Parsed[ElseStatement] = {
    ts.typeOfFirst match {
      case Else => parseStatements(ts.tail) +|> { (actions, t1) =>
        ElseStatement(actions) -> t1
      }
      case _ => Null(ts)
    }
  }

  private def parseDisplay(ts: List[Token]): Parsed[DisplayStatement] = {
    ts.typeOfFirst match {
      case LParen =>
        parseExpression(ts.tail) ||> { (expression, t1) =>
          t1.typeOfFirst match {
            case RParen => Success(DisplayStatement(expression), t1.tail)
            case _ => Failure(errorMessage("display")(t1.head, ")"))
          }
        }
      case _ => Failure(errorMessage("display")(ts.head, "("))
    }
  }

  private def parseProcedure(ts: List[Token]): Parsed[ProcedureDefinition] = {
    ts.typeOfFirst(2) match {
      case List(Identifier, Takes) => parseArguments(ts.drop(2)) +||> { (arguments, t1) =>
        t1.typeOfFirst match {
          case Does => parseStatements(t1.tail) +|> { (statements, t2) =>
            ProcedureDefinition(IDENTIFIER(ts.head), arguments, statements) -> t2
          }
          case _ => Failure(errorMessage("procedure")(t1.head, "does"))
        }
      }
      case _ => Failure(errorMessage("procedure")(ts.head, "identifier, takes"))
    }
  }

  private def parseArguments(ts: List[Token], arguments: List[Argument] = List.empty): Parsed[Argument] = {
    parseExpression(ts) ||> { (expression, t1) =>
      t1.typeOfFirst match {
        case Comma =>
          parseArguments(t1.tail, arguments :+ Argument(expression))
        case _ => Success(arguments :+ Argument(expression), t1)
      }
    }
  }

  private def parseExpression(ts: List[Token]): Parsed[Expression] = {
    val Expression = (termA: Term, t1: List[Token], tokenType: TokenType) =>
      parseTerm(t1.tail) |> { (termB, t2) =>
        EXPRESSION(termA, tokenType, termB).asInstanceOf[Expression] -> t2
      }
    ts.typeOfFirst(2) match {
      case List(Identifier, LParen) => parseArguments(ts.drop(2)) +||> { (arguments, t2) =>
        t2.typeOfFirst match {
          case RParen => Success(ProcedureCall(IDENTIFIER(ts.head), arguments), t2.tail)
          case _ => Failure(errorMessage("procedure call as expression")(t2.head, (")")))
        }
      }
      case _ => parseTerm(ts) ||> { (termA, t1) =>
        t1.typeOfFirst match {
          case Plus => Expression(termA, t1, Plus)
          case Minus => Expression(termA, t1, Minus)
          case Or => Expression(termA, t1, Or)
          case _ => Success(termA, t1)
        }
      }
    }

  }

  private def parseTerm(ts: List[Token]): Parsed[Term] = {
    val Term = (factorA: Factor, t1: List[Token], tokenType: TokenType) =>
      parseFactor(t1.tail) |> { (factorB, t2) =>
        TERM(factorA, tokenType, factorB).asInstanceOf[Term] -> t2
      }
    parseFactor(ts) ||> { (factorA, t1) =>
      t1.typeOfFirst match {
        case Multiply => Term(factorA, t1, Multiply)
        case Divide => Term(factorA, t1, Divide)
        case Modulo => Term(factorA, t1, Modulo)
        case And => Term(factorA, t1, And)
        case GrThan => Term(factorA, t1, GrThan)
        case LsThan => Term(factorA, t1, LsThan)
        case GrEqThan => Term(factorA, t1, GrEqThan)
        case LsEqThan => Term(factorA, t1, LsEqThan)
        case Equiv => Term(factorA, t1, Equiv)
        case NEquiv => Term(factorA, t1, NEquiv)
        case _ => Success(factorA, t1)
      }
    }
  }

  private def parseFactor(ts: List[Token]): Parsed[Factor] = {
    val Unary = (tokenType: TokenType) => parseFactor(ts.tail) |> { (factor, t1) =>
      UNARY(tokenType, factor).asInstanceOf[Factor] -> t1
    }
    ts.typeOfFirst match {
      case LParen => parseExpression(ts.tail) ||> { (expr, t1) =>
        t1.typeOfFirst match {
          case RParen => Success(FACTOR(expr), t1.tail)
          case _ => Failure(errorMessage("factor")(t1.head, ")"))
        }
      }
      case Identifier => Success(IDENTIFIER(ts.head), ts.tail)
      case Number => Success(VALUE(ts.head), ts.tail)
      case Negate => Unary(Negate)
      case Dereference => Unary(Dereference)
      case Pointer => Unary(Pointer)
      case Minus => Unary(Minus)
      case True => ???
      case False => ???
      case _ => Failure(errorMessage("factor")(ts.head, "(, unary, or primitive"))
    }
  }

  implicit class TokenListOps(tokens: List[Token]) {
    def typeOfFirst: TokenType = {
      tokens.head.tokenType
    }

    def typeOfFirst(n: Int): List[TokenType] = {
      tokens.take(n).map(_.tokenType)
    }

    def typeOf(n: Int): TokenType = {
      tokens(n).tokenType
    }
  }

  private def errorMessage(source: String)(token: Token, expected: String = ""): String = {
    val out = s"Failed to parse $source\nFound '${token.lexeme}' of [type = ${token.tokenType}] at {line = ${token.lineNumber}}"
    if (expected != "") {
      out + s"\nExpected ($expected) at {${token.lineNumber}} instead of '${token.lexeme}'"
    } else {
      out
    }
  }
}
