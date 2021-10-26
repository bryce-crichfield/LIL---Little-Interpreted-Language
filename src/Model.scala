import Token.TokenType

object Model {

  case class Program(variableDeclarations: List[VariableDeclaration], actions: List[Action])

  case class VariableDeclaration(IDENTIFIER1: IDENTIFIER, expression: Expression)

  sealed trait Action

  case class DisplayStatement(expression: Expression) extends Action

  case class AssignmentStatement(IDENTIFIER: IDENTIFIER, expression: Expression) extends Action

  case class IfStatement(condition: Conditional,
                         actions: List[Action],
                         ElseIfStatements: Option[List[ElseIfStatement]],
                         ElseStatement: Option[ElseStatement]
                        ) extends Action

  case class ElseIfStatement(condition: Conditional, actions: List[Action])

  case class ElseStatement(actions: List[Action])

  case class WhileStatement(condition: Conditional, actions: List[Action]) extends Action

  sealed trait Expression

  case class EXPRESSION(expression: Expression, operator: TokenType, term: Term) extends Expression

  sealed trait Term extends Expression

  case class TERM(term: Term, operator: TokenType, factor: Factor) extends Term

  sealed trait Factor extends Term

  case class VALUE(value: String) extends Factor

  object VALUE {
    def apply(token: Token): VALUE =
      VALUE(token.lexeme)
  }

  case class IDENTIFIER(value: String) extends Factor

  object IDENTIFIER {
    def apply(token: Token): IDENTIFIER =
      IDENTIFIER(token.lexeme)
  }

  case class FACTOR(expression: Expression) extends Factor

  case class UNARY(operator: TokenType, factor: Factor) extends Factor

  trait Conditional

  case class CONDITIONAL(expressionA: Expression, operator: TokenType, expressionB: Expression) extends Expression


}
