package com.yahoo.hstore.procgen.parser

import scala.util.parsing.combinator.RegexParsers
import com.yahoo.hstore.procgen._
import com.yahoo.hstore.procgen.utils._
import java.io.File
import com.yahoo.hstore.procgen.proc.Procedure

abstract class ProcTerm

case class ProcName(n: String) extends ProcTerm {
  Procedure.procTermMap.put("ProcName", this)
  override def toString = n
}
case class ProcPackage(p: String) extends ProcTerm {
  Procedure.procTermMap.put("ProcPackage", this)
  override def toString = p
}
case class ProcType(t: String) extends ProcTerm {
  Procedure.procTermMap.put("ProcType", this)
  override def toString = t
}
case class ProcTable(t: List[String]) extends ProcTerm {
  Procedure.procTermMap.put("ProcTable", this)
  override def toString = t.mkString(",")
}
case class ProcTimeWindow(start: String, startMark: String, end: String, endMark: String) extends ProcTerm {
  var tw: String = ""
  var twPara: String = ""
  if (end.equals("")) {
    tw = start + startMark + "?"
    twPara = "int " + start
  } else {
    tw = start + startMark + "? AND " + end + endMark + "?"
    twPara = "int " + start + ", int " + end
  }
  Procedure.procTermMap.put("ProcTimeWindow", this)
  override def toString = start + ", " + end
}
case class ProcSelect(s: List[String]) extends ProcTerm {
  var sList: List[Tuple2[String, String]] = List()
  for (i <- 0 to s.size - 1) {
    var n2as = s(i).split(" AS ")
    if (n2as.size == 2) {
      sList = sList.:+(n2as(0), n2as(1))
    } else {
      sList = sList.:+(n2as(0), n2as(0))
    }
  }
  Procedure.procTermMap.put("ProcSelect", this)
  override def toString = s.mkString(",")
}

case class ProcWhere(w: List[List[String]]) extends ProcTerm {

  var wPara: String = ""
  var wList: List[List[Tuple2[String, Tuple2[String, String]]]] = List()
  var wStmt: String = ""
  var kStmt: String = ""

  w.foreach(f => {
    var tList: List[Tuple2[String, Tuple2[String, String]]] = List()
    var code: Array[String] = new Array[String](f.size)
    for (i <- 0 to f.size - 1) {
      code(i) = "0"
    }
    tList = tList.:+(code.mkString(""), ("", ""))

    for (i <- 0 to f.size - 1) {
      var c: Array[String] = code.clone
      c(i) = "1"
      var n2func = f(i).split("#")
      if (n2func.size == 2) {
        tList = tList.:+(c.mkString(""), (n2func(0).trim(), n2func(1).trim()))
      } else {
        tList = tList.:+(c.mkString(""), (n2func(0).trim(), ""))
      }

      if (wPara.equals("")) {
        wPara = "String " + n2func(0).trim()
      } else {
        wPara += ", String " + n2func(0).trim()
      }

      wStmt += Constants.indent + Constants.indent + "String " + n2func(0).trim() + "Tag = " + n2func(0).trim() +
        ".equals(ALL) ? \"0\" : \"1\";" + Constants.singleLine

      if (kStmt.equals("")) {
        kStmt = Constants.indent + Constants.indent + "String key = " + n2func(0).trim() + "Tag"
      } else {
        kStmt += " + " + n2func(0).trim() + "Tag"
      }
    }
    wList = wList.:+(tList)
  })
  kStmt += ";" + Constants.doubleLine

  Procedure.procTermMap.put("ProcWhere", this)
  override def toString = w.mkString(",")
}
case class ProcOrderBy(ob: List[String], o: String) extends ProcTerm {
  Procedure.procTermMap.put("ProcOrderBy", this)
  override def toString = ob.mkString(",") + o
}
case class ProcNil() extends ProcTerm {
  override def toString = "Nil"
}

object TemplateParser extends RegexParsers {

  def exp = "[^;]*".r
  def ident = "[^ \t\n\r;]+".r
  def ws = "[ \t]*".r

  // comments start with #
  def comment: Parser[String] = ".*".r;
  def EOL: Parser[String] = ws ~ ";" ~ ws ^^^ ""
  def stmt: Parser[ProcTerm] = "%name" ~> (ws ~> exp) ^^ { n => new ProcName(n) } |
    "%package" ~> (ws ~> exp) ^^ { p => new ProcPackage(p) } |
    "%type" ~> (ws ~> exp) ^^ { t => new ProcType(t) } |
    "%table" ~> (ws ~> exp) ^^ { t => new ProcTable(t.split("~").toList) } |
    "%timewindow" ~> (ws ~> ident) ~ (ws ~> ident) ~ (ws ~> ident) ~ (ws ~> ident) ^^ { case i ~ j ~ m ~ n => new ProcTimeWindow(i, j, m, n) } |
    "%timewindow" ~> (ws ~> ident) ~ (ws ~> ident) ^^ { case i ~ j => new ProcTimeWindow(i, j, "", "") } |
    "%select" ~> (ws ~> exp) ^^ { s => new ProcSelect(s.split("~").toList) } |
    "%where" ~> (ws ~> exp) ^^ { w => new ProcWhere(w.split("~").toList.map(_.split("/").toList)) } |
    "%orderby" ~> (ws ~> ident <~ "ASC") ^^ { ob => new ProcOrderBy(ident.split("~").toList, "ASC") } |
    "%orderby" ~> (ws ~> ident <~ "DESC") ^^ { ob => new ProcOrderBy(ident.split("~").toList, "DESC") } |
    "#" ~> comment ^^ { c => new ProcNil() };

  def stmtList: Parser[List[ProcTerm]] = stmt ~ (EOL ~> stmtList) ^^ { case x ~ xs => x :: xs } |
    stmt <~ EOL ^^ { x => x :: Nil } |
    stmt ~> stmtList ^^ { x => x }

  def parse(s: File): List[ProcTerm] = parseAll(stmtList, io.Source.fromFile(s).mkString) match {
    case Success(res, _) => res
    case e => throw new Exception(e.toString)
  }

  def parseFromString(str: String): List[ProcTerm] = parseAll(stmtList, str) match {
    case Success(res, _) => res
    case e => throw new Exception(e.toString)
  }
}

object testTemplateParser {
  // for test
  def main(args: Array[String]) {
    val p: List[ProcTerm] = TemplateParser.parse(new File(Config.FileName));
    p.foreach(x => println(x.toString))
  }
}







