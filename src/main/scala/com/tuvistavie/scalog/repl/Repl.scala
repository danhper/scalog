package com.tuvistavie.scalog
package repl

import parsers.DatalogParser

import engine.ExecutableQuery._
import com.tuvistavie.scalog.engine.{DefaultExecutor, Executor}
import com.tuvistavie.scalog.models.{Import, UserInput, Database, Query}

class Repl(implicit val db: Database) extends DatalogParser {
  implicit val executor: Executor = new DefaultExecutor
  implicit val basePath: String = sys.props("user.dir")

  private def handleQuery(query: Query) = {
    try {
      if (query.run) println("yes")
      else println("no")
    } catch {
      case e: Exception => println(e.getMessage)
    }
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
      parseAll(input, ln) match {
        case Success(input, _) => handleInput(input)
        case NoSuccess(msg, _) => println(msg)
      }
      print(">> ")
    }
  }
}

object Repl {
  def start(): Unit = new Repl()(Database.empty).launch()
}
