package com.tuvistavie.scalog
package engine

import com.tuvistavie.scalog.models.{Database, Query}

class ExecutableQuery(query: Query) {
  def run(implicit database: Database, executor: Executor): Inference  = executor.execute(query)
  def ?(implicit database: Database, executor: Executor): Inference  = run
}

object ExecutableQuery {
  implicit def query2ExecutableQuery(query: Query) = new ExecutableQuery(query)
}
