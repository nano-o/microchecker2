package microchecker

import org.scalatest._
import MicroCheckerLib._
import MultiPaxos4._

class PaxosTest extends FlatSpec with Matchers {
  
    val l : Logger = new Logger(false)
	  val myLTS : PaxosLTS = new PaxosLTS;
		val myMC = new SimpleModelChecker[mp_state_ext[Int, Unit], mp_action[Int]](myLTS, l);
		
	  it should "find error" in {
	    assert (myMC.check match { 
	      case Some(trace) => {
	        l.startlog()
	        myMC.printTrace(trace); 
	        l.log("=================================================================================================================================")
          var lastState = trace._1
          var lastID = myMC.getIndex(lastState)
          l.log("------------------------------------------------------------------------------------------------------------------------------")
          l.log("state: " + lastID)
          l.log(lastState.toString)
          
          trace._2 foreach { case (label,s) => {
            l.log("->->->->->->->->->->->->->->->-> msg:" + label + " ->->->->->->->->->->->->->->->->");
            val newID = myMC.getIndex(s)
            myLTS.printDiff(lastID, lastState, newID, s, l)
            lastState = s
            lastID = newID
            l.log("------------------------------------------------------------------------------------------------------------------------------")
            l.log("state: " + newID)
            l.log(s.toString)
          }
          }
	        l.closelog()
	        false}
	      case None => true
	    } )
	  }
}