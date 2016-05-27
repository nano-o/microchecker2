package microchecker

import org.scalatest._


object FinfunCount {
  
  def collect[T](t: T): Iterator[String] = t match {
    case t: MicroCheckerLib.finfun[_,_] => Iterator("ha")
    case p: Product  => p.productIterator.flatMap(collect)
    case _           => Iterator()
  }
  
}

class Test extends FlatSpec with Matchers {
  
  val testObject1 = MicroCheckerLib.finfun_const[Int,Int](0)
  it should "find finfuns" in {
    assert(FinfunCount.collect(testObject1).toSet == Set("ha"))
  }
  
  abstract class C1
  case object C1C1 extends C1
  case class C1C2(f : MicroCheckerLib.finfun[Int, Int], x : C1) extends C1
  val testObject2 = C1C2(MicroCheckerLib.finfun_const[Int,Int](0), C1C1)
  it should "find second-level finfuns" in {
    assert(FinfunCount.collect(testObject2).size == 1)
  }
  
  val testObject3 = C1C2(MicroCheckerLib.finfun_const[Int,Int](0), C1C2(MicroCheckerLib.finfun_const[Int,Int](0), C1C1))
  it should "find third-level finfuns too" in {
    assert(FinfunCount.collect(testObject3).size == 2)
  }
  
}