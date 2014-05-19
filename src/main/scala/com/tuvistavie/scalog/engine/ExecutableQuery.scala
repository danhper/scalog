package com.tuvistavie.scalog
package engine

import com.tuvistavie.scalog.models.Query

class ExecutableQuery(query: Query) {
  def ? = currentExecutor.execute(query)
}

object ExecutableQuery {
  implicit def query2ExecutableQuery(query: Query) = new ExecutableQuery(query)
}
