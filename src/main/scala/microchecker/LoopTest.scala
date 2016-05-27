package microchecker

import org.scalatest._

class LoopTest extends FlatSpec with Matchers {
	class LoopLTS extends LTS[Int,Int] { 
	  def initialStates : Set[Int] = Set(0)
		def successors(s:Int) : Set[(Int,Int)] = {
	    s match {
	      case 0 => Set(Tuple2(0,1),Tuple2(5,3))
	      case 1 => Set(Tuple2(1,2))
	      case 2 => Set(Tuple2(2,1), Tuple2(3,3))
	      case 3 => Set(Tuple2(4,0))
	    }
	  }
		def invariants = Set(s => s != 3)
		def constraints = Set()
	}
	
	
	val l : Logger = new Logger(true)
	val myMC = new SimpleModelChecker(new LoopLTS,l);

	it should "work" in {
	  assert(myMC.check == Some(0,List((5,3))))
	}
	
	object LoopLTS2 extends LoopLTS {
		override def invariants = Set(s => s != 2)
	}
	val myMC2 = new SimpleModelChecker(LoopLTS2,l);
	it should "work again" in {
	  assert(myMC2.check == Some((0,List((0,1), (1,2)))))
	}

}