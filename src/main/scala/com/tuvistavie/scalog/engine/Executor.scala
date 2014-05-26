package com.tuvistavie.scalog
package engine

import com.tuvistavie.scalog.models.{Database, Query}

trait Executor {
  def execute(query: Query)(implicit database: Database): Boolean
}

object YesExecutor extends Executor {
  def execute(query: Query)(implicit database: Database): Boolean = true
}
