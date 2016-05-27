package microchecker

object Checker {
    def main(args: Array[String]) 
  {
      val myLTS : LTS[Int, Unit] = new TestLTS(3);
      val l : Logger = new Logger(true)
      val myMC = new SimpleModelChecker[Int, Unit](myLTS,l);
      myMC.check;
 }
}