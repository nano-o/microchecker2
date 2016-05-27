package microchecker

trait ModelChecker[S,L] 
{
  def check : Option[(S,List[(L,S)])]
  def printTrace(trace : (S,List[(L,S)])) : Unit
}