package com.yahoo.hstore.procgen.utils

object Constants {

  val singleLine: String = "\n";
  val doubleLine: String = "\n\n";
  val endLine: String = ";";
  val indent: String = "\t";

  // package header
  val packageHeader: String = "package ";

  // import list
  val importList: String = """import org.voltdb.SQLStmt;
import org.voltdb.VoltProcedure;
import org.voltdb.VoltTable;
import org.voltdb.VoltType;"""

  //class header
  val classHeader: String = "public class ~ extends VoltProcedure {"

  // all statement
  val allStmt: String = """private static final String ALL = "N";"""

  // sql statement
  val sqlStmt: String = """public final SQLStmt ~ = new SQLStmt(""";

  val colStmt: String = indent(2) + """VoltTable.ColumnInfo typeCol = new VoltTable.ColumnInfo("type", VoltType.STRING);""" +
    singleLine + indent(2) + """VoltTable.ColumnInfo countCol = new VoltTable.ColumnInfo("count", VoltType.BIGINT);""" +
    singleLine + indent(2) + """VoltTable resultTable = new VoltTable(typeCol, countCol);"""

  def indent(i: Int): String = {
    var res: String = ""
    for (j <- 1 to i) {
      res += indent
    }
    res
  }

}