package com.tuvistavie.scalog.ast

sealed trait Symbol {
  def name: String
}

case class Variable(name: String) extends Symbol
case class Constant(name: String) extends Symbol
