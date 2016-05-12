package microchecker

import org.scalatest._

class PetersonTest extends FlatSpec with Matchers {

	"The model checker" should "find invariant violations" in 
	{
		val myMC = new SimpleModelChecker[State, Unit]();
		val myLTS : LTS[State, Unit] = new PetersonLTS;
		intercept[RuntimeException] {
			myMC.check(myLTS)
		}
	}
}