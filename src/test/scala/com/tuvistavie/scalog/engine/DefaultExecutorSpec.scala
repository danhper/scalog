package com.tuvistavie.scalog.engine

import com.tuvistavie.scalog.models.{Rule, Fact, Query, MapDatabase}
import com.tuvistavie.scalog.models.DatalogImplicits._
import ExecutableQuery._


import org.specs2.mutable._


class DefaultExecutorSpec extends Specification {
  implicit object DefaultDatabase extends MapDatabase(List(
    "male".a("foo"),
    "male".a("baz"),
    "female".a("bar"),
    "female".a("qux"),

    "parent".a("qux", "foo"),
    "parent".a("foo", "baz"),
    "parent".a("bar", "baz"),

    "father".a("A", "B") :- "parent".a("A", "B") ~~ "male".a("A"),
    "mother".a("A", "B") :- "parent".a("A", "B") ~~ "female".a("A"),

    "ancestor".a("A", "B") :- "parent".a("A", "B"),
    "ancestor".a("A", "B") :- "parent".a("A", "C") ~~ "ancestor".a("C", "B")
  ))

  implicit object ImplicitExecutor extends DefaultExecutor with Executor

  "The executor" should {
    "execute queries with only constants" in {
      val result: EvaluationResult = "male".a("foo").q?

      result.result must_== SimpleSuccess
      result.seenRules must haveLength(1)
    }

    "execute queries with variables" in {
      val result: EvaluationResult = "male".a("X").q?

      result.result must_== SuccessWith(List(new Substitution("X".v, "foo".c)))
      result.seenRules must haveLength(1)
    }

    "ignore already seen rules" in {
      val seenRules: List[Rule] = List("male".a("foo"))
      val result: EvaluationResult = new Query("male".a("X"), seenRules).run

      result.result must_== SuccessWith(List(new Substitution("X".v, "baz".c)))
      result.seenRules must haveLength(2)
      result.seenRules must containAllOf(seenRules)
    }

    "handle compound queries" in {
      val result = ("male".a("X") ~~ "parent".a("foo", "X")).q?

      result.result must_== SuccessWith(List(new Substitution("X".v, "baz".c)))
      result.seenRules must not beEmpty
    }

    "handle compound rules" in {
      val result = "mother".a("X", "baz").q?

      result.result must_== SuccessWith(List(new Substitution("X".v, "bar".c)))
      result.seenRules must not beEmpty
    }

    "handle recursive rules" in {
      val result = "ancestor".a("qux", "baz").q?

      result.result must_== SimpleSuccess
    }
  }
}
