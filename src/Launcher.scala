import Parser.{Failure, Success}

import java.io.File
import scala.io.Source
import scala.io.StdIn.readLine

object Launcher  {

  def main(args: Array[String]): Unit = {
    val path = args.head
    val load = Source.fromFile(new File(path))
    val tokens = Lexer(load.mkString)
    val program = Parser.parse(tokens)
    program match {
      case Success(targets, _) =>
        val interpreter = new Interpreter(targets.head)
        interpreter.run()
      case Failure(msg) => println(msg)
    }
  }




}
