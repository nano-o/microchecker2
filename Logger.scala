package microchecker

class Logger(d : Boolean) {
  val debug : Boolean = d
  def log(s : String) = println(s)
  def debug(s : String) = if (d) println(s) 
}