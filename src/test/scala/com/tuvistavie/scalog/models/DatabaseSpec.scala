package com.tuvistavie.scalog.models

import org.specs2.mutable.Specification
import com.tuvistavie.scalog.parsers.DatalogParser

class DatabaseSpec extends Specification {

  "database" should {
    "build correct map" in {
      val parsedDatabase = DatalogParser.parseResource("/dummy.pl")
      parsedDatabase must beLeft
      val database = parsedDatabase.left.get
      database.size must_== 2
      database.get(("parent", 2)) must beSome
      database.get(("ancestor", 2)) must beSome
      database(("parent", 2)) must haveSize(2)
      database(("ancestor", 2)) must haveSize(2)
    }

    "merge other databases" in {
      val database: Database = Database.empty
      val otherDatabase = DatalogParser.parseResource("/dummy.pl").left.get
      database.size must_== 0
      database merge otherDatabase
      database.size must_== 2
    }
  }

}
