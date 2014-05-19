package com.tuvistavie.scalog

import com.tuvistavie.scalog.models.Database

package object engine {
  protected[engine] implicit var currentExecutor = YesExecutor
  protected[engine] implicit val currentDatabase = Database.empty
}
