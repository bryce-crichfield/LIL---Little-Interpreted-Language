import Model._
import Token.{Divide, GrEqThan, GrThan, LsEqThan, LsThan, Minus, Multiply, Negate, Plus}

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
      vs.foreach(v => currentScope.put(v.IDENTIFIER1, interpret(v.expression)))
      tv.head match {
        case DisplayStatement(expression) =>
          println(interpret(expression))
          walk(tv.tail)
        case AssignmentStatement(identifier, expression) =>
          // simply reassigning the identifier to the expression via the current scope won't work
          // we need to recurse through the scope and find the declaration of the identifier
        case IfStatement(condition, actions, _, _) =>
          if (interpret(condition) == 1) walk(actions)
          walk(tv.tail)
        case ProcedureCall(identifier) =>
          val stmts = currentScope.procedure(identifier)
          walk(stmts)
          walk(tv.tail)
        case WhileStatement(condition, actions) =>
          currentScope = new Scope("while statement", Some(currentScope))
          while(interpret(condition) == 1) walk(actions)
          currentScope = currentScope.parentScope.get
          walk(tv.tail)
      }
      }
    }


  private def interpret(expression: Expression): Int = {
    expression match {
      case EXPRESSION(expression, operator, term) => expression match {
        case t : Term => operator match {
          case Plus => interpret(t) + interpret(term)
          case Minus => interpret(t) - interpret(term)
        }
        case EXPRESSION(expression, operator, term) => operator match {
          case Plus => interpret(expression) + interpret(term)
          case Minus => interpret(expression) - interpret(term)
        }
      }
      case f: FACTOR => interpret(f)
      case t: TERM => interpret(t)
      case VALUE(value) => value.toInt
      case i: IDENTIFIER => currentScope.variable(i)
    }
  }

  private def interpret(term: Term): Int = {
    term match {
      case TERM(term, operator, factor) => term match {
        case t: TERM => operator match {
          case Multiply => interpret(t) * interpret(factor)
          case Divide => interpret(t) / interpret(factor)
          case GrThan => (interpret(t) > interpret(factor)).toInt
          case GrEqThan => (interpret(t) >= interpret(factor)).toInt
          case LsThan => (interpret(t) < interpret(factor)).toInt
          case LsEqThan => (interpret(t) <= interpret(factor)).toInt
        }
        case f: Factor => operator match {
          case Multiply => interpret(f) * interpret(factor)
          case Divide => interpret(f) / interpret(factor)
          case GrThan => (interpret(f) > interpret(factor)).toInt
          case GrEqThan => (interpret(f) >= interpret(factor)).toInt
          case LsThan => (interpret(f) < interpret(factor)).toInt
          case LsEqThan => (interpret(f) <= interpret(factor)).toInt
        }
      }
      case VALUE(value) => value.toInt
      case i: IDENTIFIER => currentScope.variable(i)
    }
  }

  // Todo: add id support
  private def interpret(factor: Factor): Int = {
    factor match {
      case id: IDENTIFIER => currentScope.variable(id)
      case FACTOR(expression) => interpret(expression)
      case VALUE(value) => value.toInt
      case UNARY(operator, factor) =>
        operator match {
          case Minus => -1 * interpret(factor)
        }
    }
  }

  private def interpret(If: IfStatement): Unit = {
    if(interpret(If.condition) == 1) {
      currentScope = new Scope("if statement", Some(currentScope))
      walk(If.actions)
      currentScope = currentScope.parentScope.get
    } else if(If.ElseIfStatements.isDefined) {
      val elseIfs = If.ElseIfStatements.get
      val first = elseIfs.head
      val asIf = IfStatement(first.condition, first.actions, Option(elseIfs.tail), If.ElseStatement)
      interpret(asIf)
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
