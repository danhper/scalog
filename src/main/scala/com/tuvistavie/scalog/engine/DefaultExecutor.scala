package com.tuvistavie.scalog.engine

import com.tuvistavie.scalog.models.{Atom, Database, Query}

class DefaultExecutor extends Executor {
  def execute(query: Query)(implicit database: Database): Boolean = {
    query.atoms forall executeAtom
  }

  private def executeAtom(atom: Atom)(implicit database: Database): Boolean = {
    database.getRule(atom.name, atom.arity) match {
      case Some(rule) => true
      case None => throw new ExecutionException(s"no rule ${atom.name}/${atom.arity}")
    }
  }
}
