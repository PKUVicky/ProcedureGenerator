package com.yahoo.hstore.procgen

import com.yahoo.hstore.procgen._
import com.yahoo.hstore.procgen.parser._
import com.yahoo.hstore.procgen.proc.Procedure

object ProcGen extends App {

  override def main(args: Array[String]) = {
    Procedure.generate
  }

}