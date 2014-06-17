package com.yahoo.hstore.procgen.proc

import com.yahoo.hstore.procgen.parser._
import com.yahoo.hstore.procgen.utils.Constants
import com.yahoo.hstore.procgen.utils.Config
import java.io.Writer
import java.io.FileWriter
import java.io.File

class SimpleProcedure extends Procedure {
  def generate(p: List[ProcTerm]) {

    // generate class and package
    val procPackage: ProcTerm = Procedure.procTermMap.getOrElse("ProcPackage", new ProcPackage(""));
    val procName: ProcTerm = Procedure.procTermMap.getOrElse("ProcName", new ProcName(""));

    Procedure.res += Constants.packageHeader + procPackage.asInstanceOf[ProcPackage].p +
      Constants.endLine + Constants.doubleLine + Constants.importList + Constants.doubleLine +
      Constants.classHeader.replace("~", procName.asInstanceOf[ProcName].n) + Constants.doubleLine +
      Constants.indent + Constants.allStmt + Constants.singleLine

    // generate sql statement
    val procSelect: ProcTerm = Procedure.procTermMap.getOrElse("ProcSelect", null);
    val procTable: ProcTerm = Procedure.procTermMap.getOrElse("ProcTable", null);
    val procWhere: ProcTerm = Procedure.procTermMap.getOrElse("ProcWhere", null);
    val procTimeWindow: ProcTerm = Procedure.procTermMap.getOrElse("ProcTimeWindow", null);

    var fromStmt: String = ""
    if (procTable == null) {
      println("No table provided, stop generate");
      return ;
    } else {
      fromStmt = " From " + procTable.asInstanceOf[ProcTable].t.mkString(",")
    }

    var selectStmt: String = ""
    if (procSelect != null) {
      selectStmt = "SELECT " + procSelect.asInstanceOf[ProcSelect].s.mkString(",")
    } else {
      selectStmt = "SELECT *"
    }

    Procedure.res += Constants.indent + "private static final String SELECT_FROM_STMT = \"" +
      selectStmt + fromStmt + "\";" + Constants.doubleLine;

    var whereStmt: String = ""
    if (procTimeWindow != null) {
      whereStmt = " WHERE " + procTimeWindow.asInstanceOf[ProcTimeWindow].tw + " "
    }

    if (procWhere != null) {
      Procedure.genWhere("", "", "", "", procWhere.asInstanceOf[ProcWhere].wList, 0)
      Procedure.whereMap.foreach(wm => {

        if (whereStmt.equals("") && !wm._2.equals("")) {
          Procedure.res += Constants.indent +
            Constants.sqlStmt.replace("~", Procedure.vMap.getOrElse(wm._1, "") + "_STMT")
          Procedure.res += "SELECT_FROM_STMT + \"" +
            " WHERE " + wm._2 + ";\");" + Constants.doubleLine;
        } else {
          Procedure.res += Constants.indent +
            Constants.sqlStmt.replace("~", Procedure.vMap.getOrElse(wm._1, "") + "_STMT")
          Procedure.res += "SELECT_FROM_STMT + \"" + whereStmt +
            " AND " + wm._2 + ";\");" + Constants.doubleLine;
        }
      })
    }

    // generate run method
    Procedure.res += Constants.indent + "public VoltTable[] run(" +
      procTimeWindow.asInstanceOf[ProcTimeWindow].twPara
    if (procWhere != null) {
      if (procTimeWindow.asInstanceOf[ProcTimeWindow].twPara.equals("")) {
        Procedure.res += procWhere.asInstanceOf[ProcWhere].wPara
      } else {
        Procedure.res += ", " + procWhere.asInstanceOf[ProcWhere].wPara
      }
    }
    Procedure.res += "){" + Constants.doubleLine

    Procedure.res += procWhere.asInstanceOf[ProcWhere].wStmt + procWhere.asInstanceOf[ProcWhere].kStmt

    // generate switch statement
    var switchStmt: String = Constants.indent(2) + "switch (Integer.parseInt(key)) {" + Constants.singleLine
    Procedure.ifMap.foreach(im => {
      switchStmt += Constants.indent(2) + "case " + im._1 + ": " + Constants.singleLine + Constants.indent(3)

      if (procTimeWindow == null || procTimeWindow.asInstanceOf[ProcTimeWindow].toString.equals("")) {
        if (im._2.equals("")) {
          switchStmt += "voltQueueSQL(" + Procedure.vMap.getOrElse(im._1, "") + "_STMT);" +
            Constants.singleLine + Constants.indent(3) + "break;" + Constants.singleLine
        } else {
          switchStmt += "voltQueueSQL(" + Procedure.vMap.getOrElse(im._1, "") + "_STMT, " + im._2 + ");" +
            Constants.singleLine + Constants.indent(3) + "break;" + Constants.singleLine
        }
      } else {
        if (im._2.equals("")) {
          switchStmt += "voltQueueSQL(" + Procedure.vMap.getOrElse(im._1, "") + "_STMT, " +
            procTimeWindow.asInstanceOf[ProcTimeWindow].toString + ");" +
            Constants.singleLine + Constants.indent(3) + "break;" + Constants.singleLine
        } else {
          switchStmt += "voltQueueSQL(" + Procedure.vMap.getOrElse(im._1, "") + "_STMT, " +
            procTimeWindow.asInstanceOf[ProcTimeWindow].toString + ", " + im._2 + ");" +
            Constants.singleLine + Constants.indent(3) + "break;" + Constants.singleLine
        }
      }
    })
    Procedure.res += switchStmt + Constants.singleLine + Constants.indent(2) + "}" + Constants.doubleLine
    Procedure.res += Constants.indent + "}" + Constants.singleLine + "}"
    println(Procedure.res)

    // generate output file
    if (Config.genFile) {
      var file: File = new File(Config.outputPath + Config.pathDelimeter +
        procName.asInstanceOf[ProcName].n + ".java");
      var writer: Writer = new FileWriter(file);
      writer.write(Procedure.res);
      writer.flush;
      writer.close();
    }
  }
}
