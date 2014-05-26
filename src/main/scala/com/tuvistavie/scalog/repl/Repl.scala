package com.tuvistavie.scalog
package repl

import parsers.DatalogParser

import engine.ExecutableQuery._
import com.tuvistavie.scalog.engine.{DefaultExecutor, YesExecutor, Executor}
import com.tuvistavie.scalog.models.Database

class Repl(implicit val db: Database) extends DatalogParser {
  implicit val executor: Executor = new DefaultExecutor

  def launch(): Unit = {
    print(">> ")
    for (ln <- io.Source.stdin.getLines()) {
      parseAll(query, ln) match {
        case Success(query, _) =>
          try {
            if (query.run) println("yes")
            else println("no")
          } catch {
            case e: Exception => println(e.getMessage)
          }
        case NoSuccess(msg, _) =>
          println(msg)
      }
      print(">> ")
    }
  }
}

object Repl {
  def start(databasePath: String): Unit = {
    DatalogParser.parseFile(databasePath) match {
      case Left(db) => new Repl()(db).launch()
      case Right(msg) =>
        System.err.println(msg)
        System.exit(1)
    }
  }
}
