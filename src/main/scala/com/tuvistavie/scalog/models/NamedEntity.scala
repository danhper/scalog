package com.tuvistavie.scalog.models

trait NamedEntity {
  def name: String

  def regexp: String
  def isValidName(name: String): Boolean = name.matches(regexp)

  def checkName(name: String) {
    if (!isValidName(name)) {
      throw new IllegalArgumentException(
        "Invalid name for ${getClass.getSimpleName}"
      )
    }
  }

  override def toString: String = name
}
