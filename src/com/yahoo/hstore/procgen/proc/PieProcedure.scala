package com.yahoo.hstore.procgen.proc

import com.yahoo.hstore.procgen.parser.ProcWhere
import java.io.Writer
import com.yahoo.hstore.procgen.parser.ProcPackage
import java.io.FileWriter
import com.yahoo.hstore.procgen.parser.ProcTable
import com.yahoo.hstore.procgen.utils.Config
import com.yahoo.hstore.procgen.parser.ProcName
import com.yahoo.hstore.procgen.parser.ProcTerm
import com.yahoo.hstore.procgen.parser.ProcTimeWindow
import com.yahoo.hstore.procgen.parser.ProcSelect
import com.yahoo.hstore.procgen.utils.Constants
import java.io.File

class PieProcedure extends Procedure {
  def generate(p: List[ProcTerm]) {

    // generate class and package
    val procPackage: ProcTerm = Procedure.procTermMap.getOrElse("ProcPackage", new ProcPackage(""));
    val procName: ProcTerm = Procedure.procTermMap.getOrElse("ProcName", new ProcName(""));

    Procedure.res += Constants.packageHeader + procPackage.asInstanceOf[ProcPackage].p +
      Constants.endLine + Constants.doubleLine + Constants.importList + Constants.doubleLine +
      Constants.classHeader.replace("~", procName.asInstanceOf[ProcName].n) + Constants.doubleLine +
      Constants.indent + Constants.allStmt + Constants.doubleLine

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

    var whereStmt: String = ""
    if (procTimeWindow != null) {
      whereStmt = " WHERE " + procTimeWindow.asInstanceOf[ProcTimeWindow].tw + " "
    }

    if (procWhere != null) {
      Procedure.genWhere("", "", "", "", procWhere.asInstanceOf[ProcWhere].wList, 0)
      Procedure.whereMap.foreach(wm => {

        if (whereStmt.equals("") && !wm._2.equals("")) {
          Procedure.res += Constants.indent + Constants.sqlStmt.replace("~", "sql" + wm._1)
          Procedure.res += "\"" + selectStmt + fromStmt +
            " WHERE " + wm._2 + ";\");" + Constants.doubleLine;
        } else {
          Procedure.res += Constants.indent + Constants.sqlStmt.replace("~", "sql" + wm._1)
          Procedure.res += "\"" + selectStmt + fromStmt + whereStmt +
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

    // generate if statement
    var ifStmt: String = ""
    Procedure.ifMap.foreach(im => {
      if (!ifStmt.equals("")) {
        ifStmt += " else "
      } else {
        ifStmt = Constants.indent(2)
      }
      ifStmt += "if (key.equals(\"" + im._1 + "\")) {" + Constants.singleLine + Constants.indent(3)

      if (procTimeWindow == null || procTimeWindow.asInstanceOf[ProcTimeWindow].toString.equals("")) {
        if (im._2.equals("")) {
          ifStmt += "voltQueueSQL(sql" + im._1 + ");" +
            Constants.singleLine + Constants.indent(2) + "}"
        } else {
          ifStmt += "voltQueueSQL(sql" + im._1 + ", " + im._2 + ");" +
            Constants.singleLine + Constants.indent(2) + "}"
        }
      } else {
        if (im._2.equals("")) {
          ifStmt += "voltQueueSQL(sql" + im._1 + ", " +
            procTimeWindow.asInstanceOf[ProcTimeWindow].toString + ");" +
            Constants.singleLine + Constants.indent(2) + "}"
        } else {
          ifStmt += "voltQueueSQL(sql" + im._1 + ", " +
            procTimeWindow.asInstanceOf[ProcTimeWindow].toString + ", " + im._2 + ");" +
            Constants.singleLine + Constants.indent(2) + "}"
        }
      }
    })
    Procedure.res += ifStmt + Constants.doubleLine

    // generate excute statement
    Procedure.res += Constants.indent(2) + "VoltTable[] result = voltExecuteSQL(true);" + Constants.singleLine
    Procedure.res += Constants.indent(2) + "VoltTable table = result[0];" + Constants.singleLine

    if (procSelect != null && procSelect.asInstanceOf[ProcSelect].sList.size >= 1) {
      for (i <- 0 to procSelect.asInstanceOf[ProcSelect].sList.size - 1) {
        Procedure.res += Constants.indent(2) + "Long " + procSelect.asInstanceOf[ProcSelect].sList(i)._2 +
          " = 0L;" + Constants.singleLine
      }
    }

    Procedure.res += Constants.indent(2) + "if (table.advanceRow()) {" + Constants.singleLine
    if (procSelect != null && procSelect.asInstanceOf[ProcSelect].sList.size >= 1) {
      for (i <- 0 to procSelect.asInstanceOf[ProcSelect].sList.size - 1) {
        Procedure.res += Constants.indent(3) + procSelect.asInstanceOf[ProcSelect].sList(i)._2.trim() +
          " = table.getLong(\"" + procSelect.asInstanceOf[ProcSelect].sList(i)._2.trim() + "\");" + Constants.singleLine
      }
    }

    Procedure.res += Constants.indent(2) + "}" + Constants.doubleLine + Constants.colStmt + Constants.doubleLine

    if (procSelect != null && procSelect.asInstanceOf[ProcSelect].sList.size >= 1) {
      Procedure.res += Constants.indent(2) + "if ("
      var condList: List[String] = List()
      for (i <- 0 to procSelect.asInstanceOf[ProcSelect].sList.size - 1) {
        condList = condList.:+(procSelect.asInstanceOf[ProcSelect].sList(i)._2 + " <= VoltType.NULL_BIGINT")
      }
      Procedure.res += condList.mkString(" || ") + ")" + Constants.singleLine +
        Constants.indent(3) + "return new VoltTable[] {resultTable};" + Constants.doubleLine
    }

    if (procSelect != null && procSelect.asInstanceOf[ProcSelect].sList.size >= 1) {
      for (i <- 0 to procSelect.asInstanceOf[ProcSelect].sList.size - 1) {
        Procedure.res += Constants.indent(2) + "resultTable.addRow(new Object[] {\"" +
          procSelect.asInstanceOf[ProcSelect].sList(i)._2.trim() + "\", " +
          procSelect.asInstanceOf[ProcSelect].sList(i)._2.trim() + "});" + Constants.singleLine
      }
    }

    Procedure.res += Constants.indent(2) + "return new VoltTable[] { resultTable};" +
      Constants.singleLine + Constants.indent + "}" + Constants.singleLine + "}"
    println(Procedure.res)

    if (!Config.outputPath.equals("")) {
      var file: File = new File(Config.outputPath + Config.pathDelimeter +
        procName.asInstanceOf[ProcName].n + ".java");
      var writer: Writer = new FileWriter(file);
      writer.write(Procedure.res);
      writer.flush;
      writer.close();
    }
  }

}

