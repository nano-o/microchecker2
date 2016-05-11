package microchecker

object Checker {
    def main(args: Array[String]) 
  {
      val myMC = new SimpleModelChecker[Int, Unit]();
      val myLTS : LTS[Int, Unit] = new TestLTS(3);
      myMC.check(myLTS);
 }
}