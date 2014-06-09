package com.tuvistavie.scalog.engine

import com.tuvistavie.scalog.models._

sealed trait InferenceResult

case class SuccessWith(atoms: List[Substitution]) extends InferenceResult
case object SimpleSuccess extends InferenceResult
case object SimpleFailure extends InferenceResult


class Substitution(val source: Symbol, val target: Symbol) {
  override def toString: String = source + " = " + target
}

class DefaultExecutor extends Executor {
  def execute(query: Query)(implicit database: Database): InferenceResult = {
    processFormula(query formula)
  }

  private def processFormula(formula: Formula)(implicit database: Database): InferenceResult = {
    processAtoms(formula atoms, List.empty) match {
      case (true, Nil)  => SimpleSuccess
      case (true, list) => SuccessWith(list)
      case (false, _)   => SimpleFailure
    }
  }


  def processAtoms(atoms: List[Atom], substitutions: List[Substitution])(implicit database: Database): (Boolean, List[Substitution]) = {
    atoms match {
      case Nil     => (true, substitutions)
      case x :: xs =>
        val (success, substitution) = unifyAtom(x)
        if (!success) return (false, List.empty)
        substitution match {
          case Nil => processAtoms(xs, substitutions)
          case sub =>
            val newAtoms = sub.foldLeft(xs) { case (ats, s) =>
              ats map { a => a.substitute(s.source, s.target) }
            }
            processAtoms(newAtoms, substitutions ++ sub)
        }
    }
  }

  private def unifyAtom(atom: Atom)(implicit database: Database): (Boolean, List[Substitution]) = {
    def tryUnify(rules: List[Rule]): (Boolean, List[Substitution]) = rules match {
      case Nil     => (false, List.empty)
      case x :: xs => x match {
        case Fact(fact) => unifyArguments(atom arguments, fact arguments, List.empty) match {
          case (false, _) => tryUnify(xs)
          case success => success
        }
        case Clause(head, formula) => (false, List.empty)
      }
    }
    tryUnify(getRules(atom))
  }

  def unifyArguments(args: List[Symbol], ruleArgs: List[Symbol], substitutions: List[Substitution]): (Boolean, List[Substitution]) = {
    (args, ruleArgs) match {
      case (Nil, _) | (_, Nil) => (true, substitutions)
      case (x :: xs, y :: ys) =>
        val (success, substitution) = unify(x, y)
        if (!success) return (false, List.empty)
        substitution match {
          case None => unifyArguments(xs, ys, substitutions)
          case Some(sub) =>
            val newArgs: List[Symbol] = xs map { s => if (s == sub.source) sub.target else s }
            unifyArguments(newArgs, ys, substitutions :+ sub)
        }
    }
  }

  private def getRules(atom: Atom)(implicit database: Database): List[Rule] = {
    database.getRule(atom.name, atom.arity) match {
      case Some(rules) => rules
      case None        => throw new ExecutionException(s"no rule ${atom.name}/${atom.arity}")
    }
  }


  private def unify(premise: Symbol, fact: Symbol): (Boolean, Option[Substitution]) = {
    (premise, fact) match {
      case (c1: Constant, c2: Constant) => if (c1 == c2) (true, None) else (false, None)
      case (c1: Variable, c2: Constant) => (true, Some(new Substitution(c1, c2)))
      case _ => (false, None)
    }
  }
}
