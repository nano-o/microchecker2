package microchecker

import java.io._
import java.util._
import java.text._

class Logger(d : Boolean) {
  val debug : Boolean = d
  
  def log(s : String) = {
    println(s)
    writer.write(s + '\n')
  }
  def debug(s : String) = if (d) println(s) 
  
  var writer : PrintWriter = _
  
  def startlog() = 
  {
    val startTime = System.currentTimeMillis()
    val sdf = new SimpleDateFormat("MMM_dd_yyyy_HH_mm");    
    val resultdate = new Date(startTime);
    val filePath = "./log/" + sdf.format(resultdate);
    var file = new File(filePath);
    var fwriter = new FileWriter(file, true);
    writer = new PrintWriter(fwriter)
  }
  
  def closelog() = writer.close();
}