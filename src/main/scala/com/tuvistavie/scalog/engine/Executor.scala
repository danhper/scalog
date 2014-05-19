package com.tuvistavie.scalog
package engine

import models.Query

trait Executor {
  def execute(query: Query): Boolean
}

object YesExecutor extends Executor {
  def execute(query: Query): Boolean = true
}
