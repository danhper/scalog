package com.tuvistavie.scalog
package parsers

import scala.util.parsing.combinator.RegexParsers

import models._
import java.io.{Reader, InputStreamReader}

trait DatalogParser extends RegexParsers {

  def input: Parser[UserInput] = (fileImport | query) <~ "."

  def fileImport: Parser[Import] = "[" ~> """[a-zA-Z0-9]+""".r <~ "]" ^^ { Import(_) }

  def query: Parser[Query] = formula ^^ { Query(_) }

  def database: Parser[Database] = rep(data) ^^ { Database(_) }

  def data: Parser[Rule] = (clause | fact) <~ "."

  def fact: Parser[Fact] = atom ^^ { Fact(_) }

  def clause: Parser[Clause] = (atom <~ ":-") ~ formula ^^ {
    case at ~ form => Clause(at, form)
  }

  def formula: Parser[Formula] = repsep(atom, ",") ^^ { Formula(_) }

  def atom: Parser[Atom] = predicate ~ (parametersList ?) ^^ {
    case predicate ~ None => Atom(predicate, List.empty)
    case predicate ~ Some(params) => Atom(predicate, params)
  }

  def parametersList: Parser[List[Symbol]] = "(" ~> parameters <~ ")"

  def parameters: Parser[List[Symbol]] = rep1sep(symbol, ",")

  def predicate: Parser[Predicate] = Predicate.regexp.r ^^ { Predicate(_) }

  def symbol: Parser[Symbol] = variable | constant
  def variable: Parser[Variable] = Variable.regexp.r ^^ { Variable(_) }
  def constant: Parser[Constant] = Constant.regexp.r ^^ { Constant(_) }
}

object DatalogParser extends DatalogParser {
  def parseReader(reader: Reader): Either[Database, String] = {
    parseAll(database, reader) match {
      case Success(database, _) => Left(database)
      case NoSuccess(msg, _)    => Right(msg)
    }
  }

  def parseResource(resourceName: String): Either[Database, String] = {
    val reader = new InputStreamReader(getClass.getResourceAsStream(resourceName))
    parseReader(reader)
  }

  def parseFile(path: String): Either[Database, String] = {
    try {
      val reader = io.Source.fromFile(path).reader
      parseReader(reader)
    } catch {
      case e: Exception => Right(e.getMessage)
    }
  }
}