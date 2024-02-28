package wacc

import scala.io.Source
import java.io.File
import java.io.FileNotFoundException

import ast.Program
import ExitCodes._
import parser._
import PrettyPrint.prettyPrint
import SemanticCheck.semanticCheck
import WaccError._

object ExitCodes {
  val SUCCESS_CODE        = 0
  val SYNTAX_ERROR_CODE   = 100
  val SEMANTIC_ERROR_CODE = 200
  val FILE_ERROR_CODE     = 1
}

object Main {
  // Runs the given file through the parser and semantic checker
  // Returns a tuple of an optional list of WaccErrors and an exit code
  def runFile(filename: String): (Option[WaccErrorReport], Int) = {
    runSyntaxCheck(filename).map(runSemanticCheck(_, filename)).merge
  }

  // Runs the syntax check on the given file
  // Returns either a tuple (WaccErrorReport, exit code) or the AST
  private def runSyntaxCheck(filename: String): Either[(Option[WaccErrorReport], Int), Program] = {
    parser.parseFile(new File(filename)) match {
      case scala.util.Failure(exception) => 
        Left((Some(new FileNotFound(filename)), FILE_ERROR_CODE))
      case scala.util.Success(ast) => ast match {
        case parsley.Failure(waccError) => 
          Left((Some(new WaccErrorReport(filename, List(waccError))), SYNTAX_ERROR_CODE))
        case parsley.Success(program) => Right(program)
      }
    }
  }

  // Runs the semantic checker on the given AST
  // Returns a tuple of an optional list of WaccErrors and an exit code
  private def runSemanticCheck(ast: Program, filename: String): (Option[WaccErrorReport], Int) = {
    semanticCheck(ast) match {
      case Some(errs) => (Some(new WaccErrorReport(filename, errs)), SEMANTIC_ERROR_CODE)
      case None => (None, SUCCESS_CODE)
    }
  }

  def main(args: Array[String]): Unit = {
    // Read command line arguments (file name) and pass as a file object
    // example filename: src/test/wacc/skip.wacc
    val (waccErrors, exitCode) = args match {
      case Array(filename) => runFile(filename)
      case _ => (Some(ArgumentEmpty), FILE_ERROR_CODE)
    }
    if (waccErrors.isDefined) prettyPrint(waccErrors.get)
    sys.exit(exitCode)
  }

}
