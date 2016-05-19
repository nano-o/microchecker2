package microchecker

import org.scalatest._
import MicroCheckerLib._
import MultiPaxos4._

class PaxosTest extends FlatSpec with Matchers {
  
    val l : Logger = new Logger(true)
    
	  val myLTS : LTS[mp_state_ext[Int, Unit], mp_action[Int]] = new PaxosLTS;
		val myMC = new SimpleModelChecker[mp_state_ext[Int, Unit], mp_action[Int]](myLTS, l);
	  it should "find error" in {
	    assert (myMC.check match { 
	      case Some(trace) => {myMC.printTrace(trace);false}
	      case None => true
	    } )
	  }
  
}