package microchecker

trait ModelChecker[S,L] 
{
  def check : Boolean
}