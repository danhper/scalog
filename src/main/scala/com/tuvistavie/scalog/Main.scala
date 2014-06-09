package com.tuvistavie.scalog

import com.tuvistavie.scalog.repl.Repl

object Main {
  def usage() = {
    println("usage: scalog")
    System.exit(1)
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 0) {
      usage()
    } else {
      Repl.start()
    }
  }
}
