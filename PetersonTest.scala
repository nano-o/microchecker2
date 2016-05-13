package microchecker

import org.scalatest._

class PetersonTest extends FlatSpec with Matchers {
    val l : Logger = new Logger(false)
    
	  val myLTS : LTS[State, Unit] = new PetersonLTS;
		val myMC = new SimpleModelChecker[State, Unit](myLTS,l);
	  it should "return not find any error" in {
	    assert (myMC.check match { 
	      case Some(_) => false
	      case None => true
	    } )
	  }
	  
	  class PetersonLTS2 extends PetersonLTS {
	    override def invariants = Set(s => (!(s.step1 == 2 & s.step2 == 3)));
	  }
	  val myLTS2 : LTS[State, Unit] = new PetersonLTS2;
		val myMC2 = new SimpleModelChecker[State, Unit](myLTS2,l);
	  it should "return find a violation" in {
	    assert (myMC2.check match { 
	      case Some(trace) => trace._2.size == 11
	      case None => false
	    } )
	  }
	  
	  class PetersonLTS3 extends PetersonLTS {
	    override def invariants = Set(s => (!(s.step1 == 3 & s.step2 == 5)));
	  }
	  val myLTS3 : LTS[State, Unit] = new PetersonLTS3;
		val myMC3 = new SimpleModelChecker[State, Unit](myLTS3,l);
	  it should "find a violation again" in 
	    assert (myMC3.check match { 
	      case Some(trace) => trace._2.size == 14
	      case None => false
	    } )
}