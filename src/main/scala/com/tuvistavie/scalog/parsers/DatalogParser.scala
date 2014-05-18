package com.tuvistavie.scalog
package parsers

import scala.util.parsing.combinator.RegexParsers

import ast._

trait DatalogParser extends RegexParsers {
  val variableRegexp  = """[A-Z][a-z0-9_]+""".r
  val constantRegexp  = """[a-z][a-z0-9_]+""".r
  val predicateRegexp = constantRegexp

  def query: Parser[Query] = formula ^^ { Query(_) }

  def database: Parser[Database] = rep(data) ^^ { Database(_) }

  def data: Parser[Data] = clause | formula

  def clause: Parser[Clause] = (atom <~ ":-") ~ formula ^^ {
    case at ~ form => Clause(at, form)
  }

  def formula: Parser[Formula] = repsep(atom, ",") ^^ { Formula(_) }

  def atom: Parser[Atom] = (predicate <~ "(") ~ parameters <~ ")" ^^ {
    case pred ~ params => Atom(pred, params)
  }

  def parameters: Parser[List[Symbol]] = repsep(symbol, ",")

  def predicate: Parser[Predicate] = predicateRegexp ^^ { Predicate(_) }

  def symbol: Parser[Symbol] = variable | constant
  def variable: Parser[Variable] = variableRegexp ^^ { Variable(_) }
  def constant: Parser[Constant] = constantRegexp ^^ { Constant(_) }
}
