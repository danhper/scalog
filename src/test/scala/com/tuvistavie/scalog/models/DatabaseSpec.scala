package com.tuvistavie.scalog.models

import org.specs2.mutable.Specification
import com.tuvistavie.scalog.parsers.DatalogParser

class DatabaseSpec extends Specification {

  "database" should {
    "build correct map" in {
      val parsedDatabase = DatalogParser.parseResource("/dummy.pl")
      parsedDatabase must beLeft
      val database = parsedDatabase.left.get
      database must haveSize(2)
      database must haveKey(("parent", 2))
      database must haveKey(("ancestor", 2))
      database(("parent", 2)) must haveSize(2)
      database(("ancestor", 2)) must haveSize(2)
    }
  }

}
