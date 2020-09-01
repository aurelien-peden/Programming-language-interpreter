package lamac

import scala.io.Source

object Lamac extends App {
  if (args.length < 1) {
    System.err.println("Needs a filename")
    System.exit(1)
  }

  Utils.using(Source.fromFile(args(0))) { file =>
    val program = fastparse.parse(file.mkString, parser.program(_))
    program match {
      case fastparse.Parsed.Success(ast, _) =>
        try {
          interpreter.eval(ast)
        } catch {
          case InterpreterException(msg) => System.err.println(msg)
        }
      case failure: fastparse.Parsed.Failure =>
        System.err.println("Parse error!")
        System.err.println(failure)
    }
  }

//  println(s"Executed for ${scala.compat.Platform.currentTime - executionStart} ms")
}
