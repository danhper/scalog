package com.tuvistavie.scalog

import com.tuvistavie.scalog.repl.Repl

object Main {
  def usage() = {
    println("usage: scalog DATABASE")
    System.exit(1)
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      usage()
    } else {
      Repl.start(args(0))
    }
  }
}
