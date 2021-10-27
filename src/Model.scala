import Token.TokenType

import javax.naming.ldap.ExtendedRequest

object Model {

  case class PROGRAM(statements: List[Statement])

  sealed trait Statement

  // procedure call needs to be a type of expression
  case class ProcedureDefinition(IDENTIFIER: IDENTIFIER,
                                 arguments: List[Argument],
                                 statements: List[Statement],
                       ) extends Statement
  case class Argument(IDENTIFIER: IDENTIFIER)
  case class ReturnStatement(expression: Expression)
  case class VariableDeclaration(IDENTIFIER1: IDENTIFIER, expression: Expression) extends Statement

  case class DisplayStatement(expression: Expression) extends Statement

  case class AssignmentStatement(IDENTIFIER: IDENTIFIER, expression: Expression) extends Statement

  case class IfStatement(condition: Expression,
                         actions: List[Statement],
                         ElseIfStatements: Option[List[ElseIfStatement]],
                         ElseStatement: Option[ElseStatement]
                        ) extends Statement

  case class ElseIfStatement(condition: Expression, actions: List[Statement])

  case class ElseStatement(actions: List[Statement])

  case class WhileStatement(condition: Expression, actions: List[Statement]) extends Statement

  sealed trait Expression extends Statement

  sealed trait Term extends Expression

  sealed trait Factor extends Term

  case class ProcedureCall(IDENTIFIER: IDENTIFIER, arguments: List[Argument]) extends Expression
  case class EXPRESSION(expression: Expression, operator: TokenType, term: Term) extends Expression

  case class TERM(term: Term, operator: TokenType, factor: Factor) extends Term

  case class FACTOR(expression: Expression) extends Factor

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

  case class UNARY(operator: TokenType, factor: Factor) extends Factor


}
