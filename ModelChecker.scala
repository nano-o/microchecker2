package microchecker

trait ModelChecker[S,L] {
  def check(lts: LTS[S,L]) : Unit
}