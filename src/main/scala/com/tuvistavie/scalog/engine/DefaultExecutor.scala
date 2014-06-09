package com.tuvistavie.scalog.engine

import com.tuvistavie.scalog.models.{Atom, Database, Query}

class DefaultExecutor extends Executor {
  def execute(query: Query)(implicit database: Database): Inference = {
    database.getRule(atom.name, atom.arity) match {
      case Some(rules) => Inference(atom, rules)
      case None => throw new ExecutionException(s"no rule ${atom.name}/${atom.arity}")
    }
    executeAtom(query.atoms head)
  }
}
