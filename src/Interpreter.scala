import Model._
import Token.{Divide, GrEqThan, GrThan, LsEqThan, LsThan, Minus, Multiply, Negate, Plus, TokenType}

import scala.annotation.tailrec
import scala.collection.mutable

class Interpreter(program: PROGRAM) {

  class Scope(val name: String, val parentScope: Option[Scope]) {
    private var variables = Map[IDENTIFIER, Int]()
    private var procedures = Map[IDENTIFIER, List[Statement]]()

    def variable(id: IDENTIFIER): Int = {
      variables.get(id) match {
        case Some(variable) => variable
        case None => parentScope match {
          case Some(scope) => scope.variable(id)
          case None => throw new RuntimeException(s"ID ${id} does not exist in scope")
        }
      }
    }
    def procedure(id: IDENTIFIER): List[Statement] = {
      procedures.get(id) match {
        case Some(procedure) => procedure
        case None => parentScope match {
          case Some(scope) => scope.procedure(id)
          case None => throw new RuntimeException(s"ID ${id} does not exist in scope")
        }
      }
    }
    def put(id: IDENTIFIER, int: Int): Unit = {
      variables += (id -> int)
    }
    def put(id: IDENTIFIER, stmts: List[Statement]): Unit = {
      procedures += (id -> stmts)
    }

    def set(id: IDENTIFIER, as: Int): Unit = {
      variables.get(id) match {
        case Some(variable) => put(id, as)
        case None => parentScope match {
          case Some(scope) => scope.set(id, as)
          case None => throw new RuntimeException(s"ID ${id} does not exist in scope")
        }
      }
    }
  }

  private var currentScope = new Scope("global", None)

  def run(): Unit = {
    walk(program.statements)
  }

  // when we enter a scope, we want to grab all the definitions that will exist in that scope
  private def walk(t: List[Statement]): Unit = {
    if(t.nonEmpty) {
      // get all the defined procedures
      val (ps, tp) = procedures(t)
      val (vs, tv) = variables(tp)
      ps.foreach(p => currentScope.put(p.IDENTIFIER, p.statements))
      vs.foreach(v => currentScope.put(v.IDENTIFIER1, interpretExpression(v.expression)))
      tv.head match {
        case DisplayStatement(expression) =>
          println(interpretExpression(expression))
          walk(tv.tail)
        case AssignmentStatement(identifier, expression) =>
          // simply reassigning the identifier to the expression via the current scope won't work
          // we need to recurse through the scope and find the declaration of the identifier
          val evaled = interpretExpression(expression)
        currentScope.set(identifier, evaled)
        walk(tv.tail)
        case stmt: IfStatement =>
         interpretIf(stmt)
        case ProcedureCall(identifier) =>
          val stmts = currentScope.procedure(identifier)
          walk(stmts)
          walk(tv.tail)
        case WhileStatement(condition, actions) =>
          currentScope = new Scope("while statement", Some(currentScope))
          if(interpretExpression(condition) == 1) {
            walk(actions)
            walk(tv)
          } else {
            currentScope = currentScope.parentScope.get
            walk(tv.tail)
          }
      }
      }
    }

  private val interpretExpression: Expression => Int = {
      case EXPRESSION(expression, operator, term) => expression match {
        case e: EXPRESSION => matchExpressionOperator(operator, interpretExpression, e, term)
        case t : Term => matchExpressionOperator(operator, interpretTerm, t, term)
      }
      case expression => interpretTerm(expression.asInstanceOf[Term])
  }

  def matchExpressionOperator[A <: Expression](tokenType: TokenType, interpret: A => Int, a: A, b: A): Int = tokenType match {
    case Plus => interpret(a) + interpret(b)
    case Minus => interpret(a) - interpret(b)
  }

  private val interpretTerm: Term => Int = {
    case TERM(term, operator, factor) => term match {
      case t: TERM => matchTermOperator(operator, interpretTerm, t, factor)
      case f: Factor => matchTermOperator(operator, interpretFactor, f, factor)
    }
    case term => interpretFactor(term.asInstanceOf[Factor])
  }

  // i don't think this will work because i need two interpret oerations
  def matchTermOperator[A <: Term](tokenType: TokenType, interpret: A => Int, termA: A, termB: A): Int = tokenType match {
    case Multiply => interpret(termA) * interpret(termB)
    case Divide => interpret(termA) / interpret(termB)
    case GrThan => (interpret(termA) > interpret(termB)).toInt
    case GrEqThan => (interpret(termA) >= interpret(termB)).toInt
    case LsThan => (interpret(termA) < interpret(termB)).toInt
    case LsEqThan => (interpret(termA) <= interpret(termB)).toInt
  }



  // Todo: add id support
  private val interpretFactor: Factor => Int = {
      case id: IDENTIFIER => currentScope.variable(id)
      case FACTOR(expression) => interpretExpression(expression)
      case VALUE(value) => value.toInt
      case UNARY(operator, factor) =>
        operator match {
          case Minus => -1 * interpretFactor(factor)
        }
  }

  private val interpretIf: IfStatement => Unit = If => {
    if(interpretExpression(If.condition) == 1) {
      currentScope = new Scope("if statement", Some(currentScope))
      walk(If.actions)
      currentScope = currentScope.parentScope.get
    } else if(If.ElseIfStatements.isDefined && If.ElseIfStatements.get.nonEmpty) {
      val elseIfs = If.ElseIfStatements.get
      val first = elseIfs.head
      val asIf = IfStatement(first.condition, first.actions, Option(elseIfs.tail), If.ElseStatement)
      interpretIf(asIf)
    } else {
      If.ElseStatement match {
        case Some(stmt) => walk(stmt.actions)
        case None => ()
      }
    }
  }


  private def procedures(tree: List[Statement]): (List[ProcedureDefinition], List[Statement]) = {
    val tuple = tree partition (_.isInstanceOf[ProcedureDefinition])
    tuple._1.map(_.asInstanceOf[ProcedureDefinition]) -> tuple._2
  }

  private def variables(tree: List[Statement]): (List[VariableDeclaration], List[Statement]) = {
    val tuple = tree partition(_.isInstanceOf[VariableDeclaration])
    tuple._1.map(_.asInstanceOf[VariableDeclaration]) -> tuple._2
  }



  implicit class BooleanOps(b: Boolean) {
    def toInt: Int =
      if(b) 1
      else 0
  }


}
