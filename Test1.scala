package microchecker

import org.scalatest._

class Test1 extends FlatSpec with Matchers {
	class TestLTS1(b:Int) extends LTS[Int, Unit]{

		val bound = b

				override def initialStates = Set(1);

		override def successors(s: Int) = {
			if (s < bound) {
				Set(((), s + 1));
			}
			else Set();
		}

		override def invariants = Set(s => s <= bound, s => s <= bound - 1);

		override def constraints = Set(s => s < 4);

	}
	
	"The model checker" should "find invariant violations" in {
		val myMC = new SimpleModelChecker[Int, Unit]();
		val myLTS : LTS[Int, Unit] = new TestLTS1(3);
		intercept[RuntimeException] {
			myMC.check(myLTS)
		}
	}
}
	
	class Test2 extends FlatSpec with Matchers {
		class TestLTS2(b:Int) extends LTS[Int, Unit]{

			val bound = b

					override def initialStates = Set(1);

			override def successors(s: Int) = {
				if (s < bound) {
					Set(((), s + 1));
				}
				else Set();
			}

			override def invariants = Set(s => s <= bound, s => s <= bound + 1);

			override def constraints = Set(s => s < 4);

		}

		"The model checker" should "not find invariant violations that do not exist" in {
			val myMC = new SimpleModelChecker[Int, Unit]();
			val myLTS : LTS[Int, Unit] = new TestLTS2(3);
			myMC.check(myLTS)
		}
}