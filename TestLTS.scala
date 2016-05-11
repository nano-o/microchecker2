package microchecker

import scala.collection.immutable

class TestLTS(b:Int) extends LTS[Int, Unit]{
  
  val bound = b
  
  override def initialStates = Set(1);
  
  override def successors(s: Int) = {
    if (s < bound) {
      Set(((), s + 1));
    }
    else Set();
  }
  
  override def invariants = Set(s => s <= bound, s => s <= bound - 1);
  
  override def constraints = Set();
  
}