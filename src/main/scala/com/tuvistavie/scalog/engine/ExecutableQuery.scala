package com.tuvistavie.scalog
package engine

import com.tuvistavie.scalog.models.{Rule, Database, Query}

class ExecutableQuery(query: Query) {
  def run(implicit database: Database, executor: Executor): EvaluationResult = executor.execute(query)
  def ?()(implicit database: Database, executor: Executor): EvaluationResult  = run(database, executor)
}

object ExecutableQuery {
  implicit def query2ExecutableQuery(query: Query) = new ExecutableQuery(query)
}
