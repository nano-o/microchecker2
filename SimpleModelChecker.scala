package microchecker

import scala.collection.mutable
import scala.collection.mutable.HashSet

class SimpleModelChecker[S,L] extends ModelChecker[S,L] {
  def check(lts: LTS[S,L]) : Unit = {
    val unexplored : mutable.Set[S] = mutable.Set();
    val explored : mutable.Set[S] = mutable.Set();
    unexplored ++= lts.initialStates
    while (!unexplored.isEmpty) {
      val sopt = unexplored.find(_ => true)
      sopt match {
        case None => throw new RuntimeException("could not find any element in a non-empty set, something's weird...")
        case Some(s) => { // TODO: constraints
          val sucs = lts.successors(s) map { case (l,s) => s } filter { s => !unexplored.contains(s) & !explored.contains(s) }
          lts.invariants foreach {i =>
            if (!i(s)) {
              throw new RuntimeException("Invariant violated: " + s)
            }
          }
          unexplored ++= sucs
          unexplored -= s
          explored += s
        }
      }
    }
  }
}