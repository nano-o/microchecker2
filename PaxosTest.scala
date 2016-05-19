package microchecker

import org.scalatest._
import MicroCheckerLib._
import MultiPaxos4._

class PaxosTest extends FlatSpec with Matchers {
  
    val l : Logger = new Logger(true)
    
	  val myLTS : PaxosLTS = new PaxosLTS;
		val myMC = new SimpleModelChecker[mp_state_ext[Int, Unit], mp_action[Int]](myLTS, l);
	  it should "find error" in {
	    assert (myMC.check match { 
	      case Some(trace) => {
	        myMC.printTrace(trace); 
	        println("=================================================================================================================================")
          var lastState = trace._1
          l.log("------------------------------------------------------------------------------------------------------------------------------")
          l.log(lastState.toString)
          
          trace._2 foreach { case (label,s) => {
            l.log("->->->->->->->->->->->->->->->-> msg:" + label + " ->->->->->->->->->->->->->->->->");
            myLTS.printDiff(lastState, s)
            lastState = s
            l.log("------------------------------------------------------------------------------------------------------------------------------")
            l.log(s.toString)
          } }
	        false}
	      case None => true
	    } )
	  }
  
}