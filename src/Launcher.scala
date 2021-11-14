import Parser.{Failure, Success}

import java.io.File
import scala.io.Source

object Launcher extends App {

  val path = "src/example.scl"

  val load = Source.fromFile(new File(path))
  val tokens = Lexer(load.mkString)
//  tokens.foreach(println)
  val program = Parser.parse(tokens)
  program match {
    case Success(targets, _) =>
//      val actions = targets.head.statements
//      println("[ ACTIONS ]")
//      actions.foreach(println)
    val interpreter = new Interpreter(targets.head)
    interpreter.run()
    case Failure(msg) => println(msg)
  }


}
