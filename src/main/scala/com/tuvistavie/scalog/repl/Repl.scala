package com.tuvistavie.scalog
package repl

import parsers.DatalogParser

import engine.ExecutableQuery._
import com.tuvistavie.scalog.engine._
import com.tuvistavie.scalog.models._
import com.tuvistavie.scalog.models.Import
import com.tuvistavie.scalog.models.Query

class Repl(implicit val db: Database) extends DatalogParser {
  implicit val executor: Executor = new DefaultExecutor
  implicit val basePath: String = sys.props("user.dir")

  private def handleQuery(query: Query): Unit = {
    try {
      handleResult(query.run, query)
    } catch {
      case e: Exception => println(e.getMessage)
    }
  }


  private def handleResult(evaluationResult: EvaluationResult, baseQuery: Query): Unit = evaluationResult match {
    case EvaluationResult(SimpleFailure, _) => println("no")
    case EvaluationResult(SuccessWith(sub), r) =>
      println(sub mkString "\n")
      // TODO: implement functionality to look for next solution
      if (io.StdIn.readLine.isEmpty) println("yes")
      else handleQuery(new Query(baseQuery.formula, r))
    case EvaluationResult(SimpleSuccess, _) => println("yes")
  }

  def handleImport(fileImport: Import) = {
    try {
      val database = Database.fromModule(fileImport.module)
      db.merge(database)
      println(s"Loaded module ${fileImport.module}")
    } catch {
      case e: Exception => System.err.println(s"Could not load ${fileImport.module}: ${e.getMessage}")
    }
  }

  private def handleInput(input: UserInput) = input match {
    case query: Query       => handleQuery(query)
    case fileImport: Import => handleImport(fileImport)
  }

  def launch(): Unit = {
    print(">> ")
    for (ln <- io.Source.stdin.getLines()) {
      if (!ln.isEmpty) {
        parseAll(input, ln) match {
          case Success(input, _) => handleInput(input)
          case NoSuccess(msg, _) => println(msg)
        }
      }
      print(">> ")
    }
  }
}

object Repl {
  def start(): Unit = new Repl()(Database.empty).launch()
}
