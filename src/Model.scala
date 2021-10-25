import Token.TokenType

object Model {

  case class Program(variableDeclarations: List[VariableDeclaration], actions: List[Action])

  case class VariableDeclaration(IDENTIFIER1: IDENTIFIER, value: VALUE) {

  }

  sealed trait Action
  case class DisplayStatement(expressions: List[Expression]) extends Action
  case class AssignmentStatement(IDENTIFIER: IDENTIFIER, expression: Expression) extends Action
  case class IfStatement(condition: Condition,
                         actions: List[Action],
                         ElseIfStatements: Option[List[ElseIfStatement]],
                         ElseStatement: Option[ElseStatement]
                        ) extends Action
  case class ElseIfStatement(condition: Condition, actions: List[Action])
  case class ElseStatement(actions: List[Action])
  case class WhileStatement(condition: Condition, actions: List[Action]) extends Action
  case class Condition(subcondition: SubCondition, conditionalArgument: Option[ConditionalArgument]) extends SubCondition
  case class ConditionalArgument(booleanOperator: BooleanOperator, subcondition: SubCondition)

  sealed trait SubCondition
  case class SubConditionalExpression(expressionA: Expression, subConditionalOperator: SubConditionalOperator, expressionB: Expression) extends SubCondition

  sealed trait BooleanOperator
  case object AND extends BooleanOperator with ExpressionOperator
  case object OR extends BooleanOperator with ExpressionOperator

  sealed trait SubConditionalOperator

  sealed trait RelationalOperator extends SubConditionalOperator // lots of these??
  case object EQUIVALENCE extends SubConditionalOperator

  // TODO: Right there isn no support for arbitrarily long expressions such as 1 + 1 + 1 + 1
  case class Expression(termA: Term, expressionArgument: Option[ExpressionArgument]) extends Element
  case class ExpressionArgument(expressionOperator: ExpressionOperator, term: Term)

  sealed trait ExpressionOperator
  case object PLUS extends ExpressionOperator
  case object MINUS extends ExpressionOperator with UnaryOperator
  case class Term(unary: Unary, termArgument: Option[TermArgument])
  case class TermArgument(termOperator: TokenType, unary: Unary)
  case class Unary(unaryOperator: Option[UnaryOperator], element: Element)

  sealed trait UnaryOperator
  case object DEREFERENCE extends UnaryOperator
  case object NEGATE extends UnaryOperator

  trait Element extends SubCondition
  case class IDENTIFIER(value: String) extends Element
  case class VALUE(value: String) extends Element
  case object TRUE extends Element
  case object FALSE extends Element

}
