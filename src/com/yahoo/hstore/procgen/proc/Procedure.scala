package com.yahoo.hstore.procgen.proc

import java.io.File
import com.yahoo.hstore.procgen.parser._
import com.yahoo.hstore.procgen.utils._
import scala.collection.mutable.Map
import java.io.Writer
import java.io.FileWriter

abstract class Procedure {
  def generate(p: List[ProcTerm])
}

object Procedure {

  var procTermMap: Map[String, ProcTerm] = Map()
  var res: String = ""
  // generate where statement in sql
  var whereMap: Map[String, String] = Map()
  // generate the voltQueueSQL statement in if or switch 
  var ifMap: Map[String, String] = Map()
  // generate the sql variable statement
  var vMap: Map[String, String] = Map()

  def generate {
    val p: List[ProcTerm] = TemplateParser.parse(new File(Config.FileName));
    val procType: ProcTerm = procTermMap.getOrElse("ProcType", null);

    var proc: Procedure = null;
    if (procType != null) {
      procType.asInstanceOf[ProcType].t match {
        case "pie" => proc = new PieProcedure
        case "column" => proc = new ColumnProcedure
        case "timeline" => proc = new TimelineProcedure
        case "simple" => proc = new SimpleProcedure
        case _ => {
          println("Error with Procedure: Class " + procType.asInstanceOf[ProcType].t + " not found.")
          return
        }
      }
    }
    proc.generate(p)
  }

  def genWhere(code: String, str: String, ifStr: String, vStr: String, w: List[List[Tuple2[String, Tuple2[String, String]]]], i: Int) {
    if (i < w.size) {
      w(i).foreach(f => {
        if (f._2._1.equals("") && f._2._2.equals("")) {
          genWhere(code + f._1, str, ifStr, vStr, w, i + 1);
        } else {
          var param: String = f._2._1
          if (!f._2._2.equals("")) {
            param = f._2._2
          }
          if (str.equals("")) {
            genWhere(code + f._1, str + param + " ", ifStr + f._2._1, vStr + f._2._1.toUpperCase(), w, i + 1);
          } else {
            genWhere(code + f._1, str + "AND " + param + " ", ifStr + ", " + f._2._1, vStr + "_" + f._2._1.toUpperCase(), w, i + 1);
          }
        }
      })
    } else {
      Procedure.whereMap.put(code, str)
      Procedure.ifMap.put(code, ifStr)
      Procedure.vMap.put(code, vStr)
    }
  }
}

