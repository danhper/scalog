package com.tuvistavie.scalog.engine

import com.tuvistavie.scalog.models._

case class EvaluationResult(result: InferenceResult, seenRules: List[Rule])

sealed trait InferenceResult

case class SuccessWith(atoms: List[Substitution]) extends InferenceResult
case object SimpleSuccess extends InferenceResult
case object SimpleFailure extends InferenceResult


class DefaultExecutor extends Executor {
  def execute(query: Query)(implicit database: Database): (EvaluationResult) = {
    processFormula(query formula, query.seenRules)
  }

  private def processFormula(formula: Formula, seenRules: List[Rule] = List.empty)(implicit database: Database): EvaluationResult = {
    processAtoms(formula atoms, List.empty, seenRules) match {
      case (true, Nil, r)  => EvaluationResult(SimpleSuccess, r)
      case (true, list, r) => EvaluationResult(SuccessWith(list), r)
      case (false, _, r)   => EvaluationResult(SimpleFailure, r)
    }
  }


  def processAtoms(atoms: List[Atom], substitutions: List[Substitution], seenRules: List[Rule])(implicit database: Database): (Boolean, List[Substitution], List[Rule]) = {
    atoms match {
      case Nil     => (true, substitutions, seenRules)
      case x :: xs =>
        val (success, substitution, newlySeenRules) = unifyAtom(x, seenRules)
        if (!success) return (false, List.empty, seenRules)
        substitution match {
          case Nil => processAtoms(xs, substitutions, seenRules)
          case sub =>
            val newAtoms = sub.foldLeft(xs) { case (ats, s) =>
              ats map { a => a.substitute(s.source, s.target) }
            }
            processAtoms(newAtoms, substitutions ++ sub, newlySeenRules ++ seenRules) match {
              case (false, _, _) =>
                if (newlySeenRules.isEmpty) (false, List.empty, seenRules)
                else processAtoms(atoms, substitutions, seenRules ++ newlySeenRules)
              case s => s
            }
        }
    }
  }

  private def unifyAtom(atom: Atom, seenRules: List[Rule])(implicit database: Database): (Boolean, List[Substitution], List[Rule]) = {

    def tryUnify(rules: List[Rule], newlySeenRules: List[Rule]): (Boolean, List[Substitution], List[Rule]) = rules match {
      case Nil     => (false, List.empty, newlySeenRules)
      case x :: xs => x match {
        case Fact(fact) => unifyArguments(fact arguments, atom arguments, List.empty) match {
          case (false, _) => tryUnify(xs, x :: newlySeenRules)
          case (true, sub) => (true, sub, x :: newlySeenRules)
        }
        case Clause(head, formula) => {
          unifyArguments(head arguments, atom arguments, List.empty) match {
            case (false, _) => tryUnify(xs, x :: newlySeenRules)
            case (true, sub) => {
              val atoms = sub.foldLeft(formula) { case (f, s) => f.substitute(s.source, s.target) }.atoms
              processAtoms(atoms, List.empty, List.empty) match {
                case success @ (true, _, _) => success
                case _ => tryUnify(xs, x :: newlySeenRules)
              }
            }
          }
        }
      }
    }
    tryUnify(getRules(atom).diff(seenRules), List.empty)
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
      case (c1: Constant, c2: Variable) => (true, Some(new Substitution(c2, c1)))
      case (c1: Variable, c2: Variable) => (true, Some(new Substitution(c1, c2)))
      case _ => (false, None)
    }
  }
}
