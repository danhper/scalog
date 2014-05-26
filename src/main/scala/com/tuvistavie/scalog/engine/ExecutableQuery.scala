package com.tuvistavie.scalog
package engine

import com.tuvistavie.scalog.models.{Database, Query}

class ExecutableQuery(query: Query) {
  def ?(implicit database: Database, executor: Executor)  = executor.execute(query)
}

object ExecutableQuery {
  implicit def query2ExecutableQuery(query: Query) = new ExecutableQuery(query)
}
