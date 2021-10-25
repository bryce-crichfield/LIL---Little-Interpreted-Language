import Parser.{Failure, Success}

import java.io.File
import scala.io.Source

object Launcher extends App {

  val path = "src/test.scl"

  val load = Source.fromFile(new File(path))
  val tokens = Lexer(load.mkString)
  tokens.foreach(println)
  val program = Parser.parse(tokens)
  program match {
    case Success(targets, tokens) =>
      val declaratons = targets.head.variableDeclarations
      val actions = targets.head.actions
      println("[ DECLARATIONS ]")
      declaratons.foreach(println)
      println("[ ACTIONS ]")
      actions.foreach(println)
    case Failure(msg) => println(msg)
  }


}
