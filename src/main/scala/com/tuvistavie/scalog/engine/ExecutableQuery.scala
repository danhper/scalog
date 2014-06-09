package com.tuvistavie.scalog
package engine

import com.tuvistavie.scalog.models.{Database, Query}

class ExecutableQuery(query: Query) {
  def run(implicit database: Database, executor: Executor): InferenceResult  = executor.execute(query)
  def ?(implicit database: Database, executor: Executor): InferenceResult  = run
}

object ExecutableQuery {
  implicit def query2ExecutableQuery(query: Query) = new ExecutableQuery(query)
}
