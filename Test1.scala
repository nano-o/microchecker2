package microchecker

import org.scalatest._



class Test1 extends FlatSpec with Matchers {
	abstract class TestLTS(b:Int) extends LTS[Int, Unit]{

		val bound = b

				override def initialStates = Set(1);

		override def successors(s: Int) = {
			if (s < bound) {
				Set(((), s + 1));
			}
			else Set();
		}

		def constraints = Set(s => s < 4);


	}
	
  val l : Logger = new Logger(true)
    
	class TestLTS1(b:Int) extends TestLTS(b) {
		override def invariants = Set(s => s <= bound, s => s <= bound - 1);
	}

	"The model checker" should "find invariant violations" in {
	  val myLTS : LTS[Int, Unit] = new TestLTS1(3);
		val myMC = new SimpleModelChecker[Int, Unit](myLTS,l);
	  assert (myMC.check == Some(1,List(((),2),((),3))))
	}
	
	class TestLTS2(b:Int) extends TestLTS(b) {
		override def invariants = Set(s => s <= bound, s => s <= bound + 1);
	}

	"The model checker" should "not find invariant violations that do not exist" in {
		val myLTS : LTS[Int, Unit] = new TestLTS2(3);
	  val myMC = new SimpleModelChecker[Int, Unit](myLTS,l);
	  assert(myMC.check == None)
	}
	
}