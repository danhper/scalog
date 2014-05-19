package com.tuvistavie.scalog
package parsers

import scala.util.parsing.combinator.RegexParsers

import models._

trait DatalogParser extends RegexParsers {

  def query: Parser[Query] = formula <~ "." ^^ { Query(_) }

  def database: Parser[Database] = rep(data) ^^ { Database(_) }

  def data: Parser[Data] = (clause | formula) <~ "."

  def clause: Parser[Clause] = (atom <~ ":-") ~ formula ^^ {
    case at ~ form => Clause(at, form)
  }

  def formula: Parser[Formula] = repsep(atom, ",") ^^ { Formula(_) }

  def atom: Parser[Atom] = predicate ~ (parametersList ?) ^^ {
    case pred ~ None => Atom(pred, List.empty)
    case pred ~ Some(params) => Atom(pred, params)
  }

  def parametersList: Parser[List[Symbol]] = "(" ~> parameters <~ ")"

  def parameters: Parser[List[Symbol]] = rep1sep(symbol, ",")

  def predicate: Parser[Predicate] = Predicate.regexp.r ^^ { Predicate(_) }

  def symbol: Parser[Symbol] = variable | constant
  def variable: Parser[Variable] = Variable.regexp.r ^^ { Variable(_) }
  def constant: Parser[Constant] = Constant.regexp.r ^^ { Constant(_) }
}
