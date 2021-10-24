import model.Model._
import parsing.{Actions, And, As, Define, Dereference, Display, Divide, EndProgram, EndWhile, Equals, Equiv, False, Identifier, If, Minus, Modulo, Multiply, Number, Or, Plus, Set, Token, TokenType, True, Variables, While}
import parsing.Token._

import scala.annotation.tailrec

object Parser {

  // A Parsed of T in an ADT that represents that result of a parsing computation on type T
  // A parsing computation wraps a type that it aims to parse such as Parsed[Statement]
  // If the computation fails to parse that type (ie the input does not conform to the grammar) then
  // the computation will return a failure on that type.
  // However, if the computation is successful, it will return a success which wraps all the found instances of that type
  // plus the remaining tokens that were captured by the parsing computation
  trait Parsed[T] {
    val targets: List[T]
    val tokens: List[Token]

    implicit val id = (t: List[Token]) => t


    // advances on success, escapes on failure
    def |>[B](f: (T, List[Token]) => (B, List[Token])): Parsed[B] = {
      this match {
        case Success(targets, tokens) =>
          val (b, tkns) = f(targets.head, tokens)
          Success(List(b), tkns)
        case Failure(msg) => Failure(msg)
      }
    }

    // takes two functions to handle the case of a success and the case of a null value
    def ?|>[B](s: (T, List[Token]) => (B, List[Token]))(n: List[Token] => (B, List[Token]))
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
    def ||>[B](f: (T, List[Token]) => Parsed[B]): Parsed[B] = {
      this match {
        case Success(targets, tokens) if targets.nonEmpty =>
          f(targets.head, tokens)
        case Failure(msg) => Failure(msg)
      }
    }


  }

  case class Success[T](targets: List[T], tokens: List[Token]) extends Parsed[T]

  object Success {
    def apply[T](t: T, tokens: List[Token]): Success[T] = {
      Success(List(t), tokens)
    }
  }

  case class Failure[T](msg: String) extends Parsed[T] {
    val targets: List[T] = List.empty[T]
    val tokens: List[Token] = List.empty[Token]
  }

  case class Null[T](tokens: List[Token]) extends Parsed[T] {
    val targets: List[T] = List.empty[T]
  }

  def parse(ts: List[Token]): Parsed[Program] = {
    ts.typeOfFirst match {
      case Variables => getVariableDeclarations(ts.tail) match {
        case Success(declarations, dTokens) => getActions(dTokens)(stopAt = EndProgram) match {
          case Success(actions, aTokens) =>
            Success(List(Program(declarations, actions)), aTokens)
          case Failure(msg) => Failure(msg)
        }
        case Failure(msg) => Failure(msg)
      }
      case _ => Failure[Program]("")
    }
  }

  @tailrec
  def getVariableDeclarations(ts: List[Token], ds: List[VariableDeclaration] = List.empty): Parsed[VariableDeclaration] = {
    ts.typeOfFirst(4) match {
      case List(Define, Identifier, As, Number) => // if the first four tokens match the pattern proceed
        val id = IDENTIFIER(ts(1).lexeme) // grab the id
        val num = VALUE(ts(3).lexeme) // grab the value
        getVariableDeclarations(ts.drop(4), ds :+ VariableDeclaration(id, num)) // recurse and look for more declarations
      case List(Actions, _*) => Success(ds, ts.tail) // if the first token is an actions keyword, we can escape this section
      case _ => Failure("Variable") // anything else is a failure
    }
  }


  def getActions(tokens: List[Token], actions: List[Action] = List.empty)(stopAt: TokenType)
  : Parsed[Action] = {
    tokens.head.tokenType match {
      case Set =>
        getSet(tokens.tail) ||> { (statement, t1) =>
          getActions(t1, actions :+ statement)(EndProgram)
        }
      case While =>
        getWhile(tokens.tail) ||> { (statement, t1) =>
          getActions(t1, actions :+ statement)(EndProgram)
        }
      case If => ???
      // getIf -> getActions
      case Display => ???
      // getDisplay -> getActions
      case stopAt => Success(actions, tokens.tail)
      case _ => Failure("Set, while, if, or display expected")
    }

  }

  def getSet(tokens: List[Token]): Parsed[AssignmentStatement] = {
    tokens.typeOfFirst(2) match {
      case List(Identifier, Equals) =>
        getExpression(tokens.drop(2)) |> { (expression, t1) =>
          val id = IDENTIFIER(tokens.head.lexeme)
          AssignmentStatement(id, expression) -> t1
        }
      case _ => Failure("Identifier or equals expected")
    }
  }

  // TODO: This won'
  def getWhile(ts: List[Token]): Parsed[WhileStatement] = {
    getCondition(ts) ||> { (cond, t2) =>
      getActions(t2)(stopAt = EndWhile) match {
        case Success(actions, t3) => Success(WhileStatement(cond, actions), t3)
        case Failure(msg) => Failure(msg)
      }
    }
  }

  def getCondition(ts: List[Token]): Parsed[Condition] = {
    // getSubCondition -?> getConditionalArguments
    getSubCondition(ts).||> { (subcond, t2) =>
      getConditionalArgument(t2).?|> { (arg, t3) =>
        Condition(subcond, Some(arg)) -> t3
      } { t4 =>
        Condition(subcond, None) -> t4
      }
    }
  }

  // TODO: This probably won't work due to the recursive call up to condition
  def getSubCondition(ts: List[Token]): Parsed[SubCondition] = {
    // condition | subconditional expression | element
    // try to get expression, if that fails try to get condition, if that fails try to get element???
    // once we get the first part we still have to get the second part fuckkkkkkkk
    getSubConditionalExpression(ts) match {
      case s: Success[SubConditionalExpression] => s.asInstanceOf[Parsed[SubCondition]]
      case Failure(_) => getElement(ts) match {
        case s: Success[Element] => s.asInstanceOf[Parsed[SubCondition]]
        case Failure(_) => getCondition(ts) match {
          case s: Success[Condition] => s.asInstanceOf[Parsed[SubCondition]]
          case Failure(_) => Failure("Sub-Condition Parse Failure")
        }
      }
    }
  }

  def getConditionalArgument(ts: List[Token]): Parsed[ConditionalArgument] = {
    ts.typeOfFirst match {
      case And => getSubCondition(ts.tail) |> { (cond, ts2) =>
        ConditionalArgument(AND, cond) -> ts2
      }
      case Or => getSubCondition(ts.tail) |> { (cond, ts2) =>
        ConditionalArgument(OR, cond) -> ts2
      }
      case _ => Null(ts)
    }
  }

  def getSubConditionalExpression(ts: List[Token]): Parsed[SubConditionalExpression] = {
    // expression ||> operator ||> expression
    getExpression(ts) ||> { (expr1, ts2) =>
      getSubConditionalOperator(ts2) ||> { (op, ts3) =>
        getExpression(ts3) |> { (expr2, ts4) =>
          SubConditionalExpression(expr1, op, expr2) -> ts4
        }
      }
    }
  }

  // TODO: Add binary boolean operators
  def getSubConditionalOperator(ts: List[Token]): Parsed[SubConditionalOperator] = {
    // equiv and stuff
    ts.typeOfFirst match {
      case Equiv => Success(EQUIVALENCE, ts.tail)
      case _ => Failure("Binary Boolean Operator Expected")
    }
  }

  def getExpression(ts: List[Token]): Parsed[Expression] = {
    getTerm(ts) ||> { (term, t1) =>
      getExpressionArgument(t1).?|> { (arg, t2) =>
        Expression(term, Some(arg)) -> t2
      } { t3 =>
        Expression(term, None) -> t3
      }
    }
  }


  def getExpressionArgument(ts: List[Token]): Parsed[ExpressionArgument] = {
    ts.typeOfFirst match {
      case Plus => getTerm(ts.tail) |> { (term, ts2) =>
        ExpressionArgument(PLUS, term) -> ts2
      }
      case Minus => getTerm(ts.tail) |> { (term, ts2) =>
        ExpressionArgument(MINUS, term) -> ts2
      }
      case _ => Null(ts)
    }
  }

  // todo: real ugly, could use a cleanup... okay cleanup done, but could prob be even better
  def getTerm(ts: List[Token]): Parsed[Term] = {
    getUnary(ts) ||> { (unary, t1) =>
      getTermArgument(t1).?|> { (arg, t2) =>
        Term(unary, Some(arg)) -> t2
      } { t3 =>
        Term(unary, None) -> t3
      }
    }
  }

  def getTermArgument(ts: List[Token]): Parsed[TermArgument] = {
    ts.typeOfFirst match {
      case Multiply => getUnary(ts.tail) |> { (unary, ts2) =>
        TermArgument(Multiply, unary) -> ts2
      }
      case Divide => getUnary(ts.tail) |> { (unary, ts2) =>
        TermArgument(Divide, unary) -> ts2
      }
      case Modulo => getUnary(ts.tail) |> { (unary, ts2) =>
        TermArgument(Modulo, unary) -> ts2
      }
      case _ => Null(ts)
    }
  }


  def getUnary(ts: List[Token]): Parsed[Unary] = {
    ts.typeOfFirst match {
      case Dereference =>
        getElement(ts.tail) |> { (element, ts2) =>
          Unary(Some(DEREFERENCE), element) -> ts2
        }
      case Minus =>
        getElement(ts.tail) |> { (element, ts2) =>
          Unary(Some(NEGATE), element) -> ts2
        }
      case _ =>
        getElement(ts) |> { (element, ts2) =>
          Unary(None, element) -> ts2
        }
    }
  }

  def getElement(ts: List[Token]): Parsed[Element] = {
    ts.head.tokenType match {
      case Number => Success(VALUE(ts.head.lexeme), ts.tail) // return success with the head token as a value, and drop it from the token stream
      case Identifier => Success(IDENTIFIER(ts.head.lexeme), ts.tail)
      case True => Success(TRUE, ts.tail)
      case False => Success(FALSE, ts.tail)
      case _ => Failure("parsing.Numeric, parsing.Identifier, parsing.True, or parsing.False expected")
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
}
